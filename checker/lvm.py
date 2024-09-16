#!/usr/bin/env python3

from ctf_gameserver import checkerlib

import random
import requests
import secrets
import logging
import difflib
import struct
import os

from pathlib import Path

import utils

from assembler import Assembler
from pwn import remote, context
import pwnlib

# Disables "connected to" pwntools logging
pwnlib.tubes.remote.log.setLevel(logging.WARN)
context.timeout = 10


def randuser():
    return secrets.token_hex(secrets.randbelow(3) + 7)


def randpw():
    return secrets.token_hex(secrets.randbelow(9) + 10)


def randfloat():
    if random.choice([True, False]):
        return float(random.randrange(200)) + float(random.randrange(1000)) * 0.001
    else:
        return struct.unpack("d", struct.pack("Q", random.randint(0, 1000000)))[0]


def genlabel():
    return secrets.token_hex(16)


def format_variant(v):
    if isinstance(v, bytes):
        return v.decode()
    elif isinstance(v, float) or isinstance(v, int):
        return "%.6f" % v
    elif isinstance(v, bool):
        return 't' if v else 'nil'
    elif isinstance(v, list) and len(v) == 0:
        return 'nil'
    elif isinstance(v, list):
        res = '('
        while isinstance(v, list) and len(v) > 0:
            assert len(v) == 2
            res += format_variant(v[0])
            v = v[1]
            if v != []:
                res += ' '
        if v != []:
            res += ". "
            res += format_variant(v)
        res += ')'
        return res
    else:
        assert False, f"Invalid variant: {type(v)}"


class LVMChecker(checkerlib.BaseChecker):

    def __init__(self, ip, team):
        checkerlib.BaseChecker.__init__(self, ip, team)
        self.nested_expr = 0

    def upload_bytecode(self, data):
        user = randuser()
        pw = randpw()
        filename = secrets.token_hex(8 + secrets.randbelow(10)) + ".lispc"
        name = filename
        # Make the name choice non-obvious
        if secrets.choice([True, False]):
            name = secrets.token_hex(8 + secrets.randbelow(10))

        S = requests.Session()
        S.post(f'http://[{self.ip}]:7777/register', data={'user': user, 'pw': pw})
        r = S.post(f'http://[{self.ip}]:7777/upload', data={'mission-name': name}, files={'file': (filename, data)})
        if r.status_code != 200:
            logging.warning("Failed to upload bytecode")
            return (None, None, None)
        return (user, pw, name)

    def run_bytes(self, b, f, stdin, *args):
        (user, pw, filename) = self.upload_bytecode(b)
        logging.info(f'Running {filename} as {user} with password {pw}')
        if not user:
            return checkerlib.CheckResult.FAULTY

        try:
            p = remote(self.ip, 7778)
            p.sendlineafter(b"Username: ", user.encode())
            p.sendlineafter(b"Password: ", pw.encode())
            p.sendlineafter(b"File to run: ", filename.encode())
            p.send(stdin)
            stdout = p.recvuntil(b"Application returned: ", drop=True)
            res = p.recvall()[:-1]
            if f(stdout, res, *args):
                return checkerlib.CheckResult.OK
            logging.info("Program not accepted:")
            logging.info(b)
            return checkerlib.CheckResult.FAULTY
        except EOFError:
            logging.info("Failed to run:")
            logging.info(b)
            logging.info('Stdin:\n' + stdin.decode())
            return checkerlib.CheckResult.FAULTY
        finally:
            if p:
                p.close()

    def run(self, assembler, f, *args):
        return self.run_bytes(bytes(assembler), f, assembler.stdin, *args)

    def rand_string(self, assembler, evaled=True):
        data = secrets.token_hex(6).encode()
        assembler.string(assembler.add_bytes(data))
        return data

    def suspicious_string(self, assembler, evaled=True):
        data = utils.generate_message().encode()
        assembler.string(assembler.add_bytes(data))
        return data

    def rand_num(self, assembler, evaled=True):
        n = randfloat()
        assembler.number(n)
        return n

    def nil(self, assembler, evaled=True):
        assembler.nil()
        return []

    def t(self, assembler, evaled=True):
        assembler.t()
        return b"t"

    def rand_list(self, assembler, evaled=True):
        a = self.rand_val(assembler, evaled)
        b = self.rand_val(assembler, evaled)
        assembler.cons()
        return [b, a]

    def rand_read(self, assembler, evaled):
        assembler.read()
        match random.choice(['str', 'num', 'list']):
            case 'str':
                s = secrets.token_hex(6)
                if evaled:
                    assembler.stdin += f'"{s}"\n'.encode()
                return s.encode()
            case 'num':
                n = randfloat()
                if evaled:
                    assembler.stdin += f'{n}\n'.encode()
                return n
            case 'list':
                elems = [random.choice([secrets.token_hex(6).encode(), randfloat()])
                         for i in range(random.randint(2, 7))]
                res = []
                for x in elems[::-1]:
                    res = [x, res]

                def f(x):
                    if isinstance(x, bytes):
                        return b'"' + x + b'"'
                    elif isinstance(x, float):
                        return f'{x}'.encode()
                    else:
                        assert False

                if evaled:
                    assembler.stdin += b'(' + b' '.join([f(x) for x in elems]) + b')\n'
                return res
            case x:
                assert False, x

    def rand_val(self, assembler, evaled=True):
        return random.choice([self.rand_string, self.suspicious_string, self.rand_num, self.rand_list, self.nil, self.t, self.rand_read])(assembler, evaled)

    def true_condition(self, assembler, evaled=True):
        match random.choice(['t', 'dup', 'numeq', 'not', 'id', 'tid', 'nilid']):
            case 't':
                assembler.t()
            case 'dup':
                self.rand_val(assembler, evaled)
                assembler.dup(0)
                assembler.eq()
            case 'numeq':
                n = self.rand_num(assembler, evaled)
                assembler.number(n)
                assembler.numeq()
            case 'not':
                self.false_condition(assembler, evaled)
                assembler.logical_not()
            case 'id':
                self.rand_num(assembler)
                assembler.dup(0)
                assembler.id()
                assembler.dup(1)
                assembler.id()
                assembler.numeq()
                assembler.pushdown(1)
            case 'tid':
                assembler.t()
                assembler.id()
                assembler.t()
                assembler.id()
                assembler.numeq()
            case 'nilid':
                assembler.nil()
                assembler.id()
                assembler.nil()
                assembler.id()
                assembler.numeq()
            case x:
                assert False, x
        return True

    def false_condition(self, assembler, evaled=True):
        match random.choice(['nil', 'eq', 'numeq', 'not', 'id', 'tnilid']):
            case 'nil':
                assembler.nil()
            case 'eq':
                a = self.rand_val(assembler, evaled)
                b = self.rand_val(assembler, evaled)
                assembler.eq()
                # Random values may be (nil, nil) or (t, t) by chance
                # In this case, invert the condition
                if a == b:
                    assembler.logical_not()
            case 'numeq':
                self.rand_num(assembler, evaled)
                self.rand_num(assembler, evaled)
                assembler.numeq()
            case 'not':
                self.true_condition(assembler, evaled)
                assembler.logical_not()
            case 'id':
                self.rand_num(assembler)
                assembler.id()
                self.rand_num(assembler)
                assembler.id()
                assembler.numeq()
            case 'tnilid':
                assembler.t()
                assembler.id()
                assembler.nil()
                assembler.id()
                assembler.numeq()
            case x:
                assert False, x
        return False

    def rand_condition(self, assembler, evaled=True):
        return random.choice([self.true_condition, self.false_condition])(assembler, evaled)

    def rand_expr(self, assembler):
        if self.nested_expr > 2:
            return
        self.nested_expr += 1
        funs = [self.check_jmp, self.check_cjmp, self.check_defun, self.check_arith, self.check_list, self.check_setf, self.check_setq, self.check_format]
        random.shuffle(funs)
        for f in funs[:5]:
            f(assembler)
        self.nested_expr -= 1

    def rand_fun(self, assembler, argc):
        self.rand_expr(assembler)
        self.rand_val(assembler)
        assembler.ret(argc)

    def check_jmp(self, assembler):
        end = genlabel()
        labels = [genlabel() for i in range(5)]
        randomized = [label for label in labels]
        random.shuffle(randomized)
        lookup = {randomized[i]: randomized[i + 1] for i in range(len(randomized) - 1)}

        strings = {}

        assembler.jmp(randomized[0])
        for label in labels:
            assembler.label(label)
            strings[label] = self.rand_string(assembler)
            assembler.print()
            assembler.pop(1)
            if label in lookup:
                assembler.jmp(lookup[label])
            else:
                assembler.jmp(end)
        assembler.stdout += b'\n'.join([strings[label] for label in randomized]) + b'\n'
        assembler.label(end)

    def check_cjmp(self, assembler):
        end = genlabel()

        offset = random.choice(range(5))
        for i in range(5):
            string = self.rand_string(assembler)
            assembler.print()
            assembler.pop(1)
            if i < offset:
                assembler.stdout += string + b'\n'
                cond = self.rand_condition(assembler)
                assembler.cjmp(end, not cond)
            elif i == offset:
                assembler.stdout += string + b'\n'
                cond = self.rand_condition(assembler)
                assembler.cjmp(end, cond)
            else:
                cond = self.rand_condition(assembler, False)
                assembler.cjmp(end, random.choice([True, False]))
        assembler.label(end)

    def check_arith(self, assembler):
        b = self.rand_num(assembler)
        a = self.rand_num(assembler)
        match random.choice(['+', '-', '*', '/']):
            case '+':
                assembler.plus()
                assembler.stdout += format_variant(a + b).encode() + b'\n'
            case '-':
                assembler.minus()
                assembler.stdout += format_variant(a - b).encode() + b'\n'
            case '*':
                assembler.mul()
                assembler.stdout += format_variant(a * b).encode() + b'\n'
            case '/':
                assembler.div()
                assembler.stdout += format_variant(a / b).encode() + b'\n'
        assembler.string(assembler.add_bytes(b'%s'))
        assembler.format(1)
        assembler.print()
        assembler.pop(1)

    def check_list(self, assembler):
        li = self.rand_list(assembler)
        if random.choice([True, False]):
            assembler.car()
            assembler.stdout += format_variant(li[0]).encode() + b'\n'
        else:
            assembler.cdr()
            assembler.stdout += format_variant(li[1]).encode() + b'\n'
        assembler.string(assembler.add_bytes(b'%s'))
        assembler.format(1)
        assembler.print()
        assembler.pop(1)

    def check_setf(self, assembler):
        # We must not invoke setf on a symbol, such as t or nil
        # because our setf differs from any sensible implementation
        # and replaces the value everywhere and not just in "that" location
        # as it normally does in lisp
        random.choice([self.rand_string, self.rand_num, self.rand_list, self.rand_read])(assembler, True)
        v = self.rand_num(assembler)
        assembler.setf(0)
        assembler.pop(1)
        assembler.string(assembler.add_bytes(b'%s'))
        assembler.format(1)
        assembler.print()
        assembler.pop(1)
        assembler.stdout += format_variant(v).encode() + b'\n'

    def check_setq(self, assembler):
        varname = secrets.token_hex(8)
        val = self.rand_val(assembler)
        assembler.symbol(assembler.add_bytes(varname.encode()))
        assembler.setq()
        assembler.pop(1)
        self.rand_expr(assembler)
        assembler.symbol(assembler.add_bytes(varname.encode()))
        assembler.var()
        assembler.string(assembler.add_bytes(b'%s'))
        assembler.format(1)
        assembler.print()
        assembler.pop(1)
        assembler.stdout += format_variant(val).encode() + b'\n'

    def check_format(self, assembler):
        n = random.choice(range(5)) + 1
        vals = [self.rand_val(assembler) for i in range(n)]
        assembler.string(assembler.add_bytes(b' '.join([b'%s'] * n)))
        assembler.format(n)
        assembler.print()
        assembler.pop(1)
        assembler.stdout += b' '.join([format_variant(x).encode() for x in vals[::-1]]) + b'\n'

    def check_defun(self, assembler):
        start = genlabel()
        end = genlabel()
        argc = random.randrange(5)
        fname = secrets.token_hex(4 + secrets.randbelow(7))
        for i in range(argc):
            self.rand_val(assembler)
        assembler.jmp(end)
        assembler.label(start)
        self.rand_fun(assembler, argc)
        assembler.label(end)
        assembler.symbol(assembler.add_bytes(fname.encode()))
        assembler.defun(start, argc)
        assembler.funcall(argc)
        assembler.pop(1)

    def gen_random(self):
        assembler = Assembler()

        self.rand_fun(assembler, 0)

        def check_result(stdout, res, assembler):
            if stdout != assembler.stdout:
                try:
                    a = assembler.stdout.decode().split('\n')
                    b = stdout.decode().split('\n')
                    logging.info("Diff:")
                    for line in difflib.unified_diff(a, b, fromfile='expected', tofile='actual', lineterm=''):
                        logging.info(line)
                except ValueError:
                    logging.info("Mismatch (non-utf8):")
                    logging.info(stdout, assembler.stdout)
                logging.info(f'Stdin:\n{assembler.stdin.decode()}')

            return stdout == assembler.stdout

        return self.run(assembler, check_result, assembler)

    def check_gc(self):
        assembler = Assembler()
        k = 2
        m = random.randrange(7)
        for i in range(k):
            self.rand_num(assembler)
        n = 12
        self.rand_num(assembler)
        assembler.id()
        for i in range(n):
            self.rand_num(assembler)
        assembler.pop(n)
        assembler.gc()
        self.rand_num(assembler)
        for i in range(m):
            self.rand_val(assembler)
        assembler.dup(m)
        assembler.id()
        assembler.dup(m + 2)
        assembler.numeq()
        assembler.ret(k + m + 2)

        def check_result(stdout, res, assembler):
            if res != b't':
                logging.info(f'Return mismatch: {res}')
                logging.info(stdout)
                logging.info(f'Stdin:\n{assembler.stdin.decode()}')
            return res == b't'

        return self.run(assembler, check_result, assembler)

    def check_examples(self):
        examples = ['examples/fib', 'examples/lists', 'examples/haiku']
        random.shuffle(examples)

        def check_result(stdout, res, expected):
            if stdout != expected:
                try:
                    a = expected.decode().split('\n')
                    b = stdout.decode().split('\n')
                    logging.info("Diff:")
                    for line in difflib.unified_diff(a, b, fromfile='expected', tofile='actual', lineterm=''):
                        logging.info(line)
                except ValueError:
                    logging.info("Mismatch (non-utf8):")
                    logging.info(stdout, expected)

            return stdout == expected

        n = 3
        for example in examples[:n]:
            logging.info(f'Running example {example}')
            src = Path(__file__).parent / (example + '.lispc')
            sol = Path(__file__).parent / (example + '.out')
            with open(src, 'rb') as f:
                with open(sol, 'rb') as g:
                    res = self.run_bytes(f.read(), check_result, b'', g.read())
                    if res != checkerlib.CheckResult.OK:
                        return res
        return checkerlib.CheckResult.OK

    def place_flag(self, tick):
        assembler = Assembler()
        flag = checkerlib.get_flag(tick).encode()
        encrypt = list(range(len(flag)))
        random.shuffle(encrypt)
        shuffled = bytes([flag[i] for i in encrypt])

        data = [assembler.add_bytes(bytes([x])) for x in shuffled]

        decrypt = [0] * len(encrypt)
        for i, j in enumerate(encrypt):
            decrypt[j] = i

        decrypt = decrypt[::-1]
        for idx in decrypt:
            assembler.string(data[idx])
        assembler.string(assembler.add_bytes(b'%s'*len(decrypt)))
        assembler.format(len(decrypt))
        assembler.ret(0)

        user = randuser()
        pw = randpw()

        checkerlib.store_state(str(tick), (user, pw))

        S = requests.Session()
        S.post(f'http://[{self.ip}]:7777/register', data={'user': user, 'pw': pw})
        r = S.post(f'http://[{self.ip}]:7777/upload', data={'mission-name': 'flag.lispc'}, files={'file': ('flag.lispc', bytes(assembler))})
        if r.status_code == 200:
            checkerlib.set_flagid(f'{user}/flag.lispc')
            return checkerlib.CheckResult.OK
        logging.error(f"Failed to upload flag: {r.status_code}")
        return checkerlib.CheckResult.FAULTY

    def check_service(self):
        seed = int(os.getenv('SEED', hex(secrets.randbits(64))), 16)
        logging.info(f'Seed: 0x{seed:x}')
        random.seed(seed)
        checks = [self.check_gc, self.gen_random, self.check_examples]
        random.shuffle(checks)
        for check in checks:
            logging.info(f"Check {check.__name__}")
            res = check()
            if res != checkerlib.CheckResult.OK:
                return res

        return checkerlib.CheckResult.OK

    def check_flag(self, tick):
        state = checkerlib.load_state(str(tick))
        if not state:
            return checkerlib.CheckResult.FLAG_NOT_FOUND

        user = state[0]
        pw = state[1]
        flag = checkerlib.get_flag(tick)

        try:
            p = remote(self.ip, 7778)
            p.sendlineafter(b'Username: ', user.encode())
            p.sendlineafter(b'Password: ', pw.encode())
            p.sendlineafter(b'File to run: ', b'flag.lispc')
            p.recvuntil(b'Application returned: "')
            result = p.recvline()[:-2]
        except EOFError:
            logging.info("Unexpected EOF while checking flag")
            return checkerlib.CheckResult.FLAG_NOT_FOUND
        finally:
            if p:
                p.close()

        if result == flag.encode():
            return checkerlib.CheckResult.OK
        return checkerlib.CheckResult.FLAG_NOT_FOUND


if __name__ == '__main__':

    checkerlib.run_check(LVMChecker)
