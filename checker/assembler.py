from pwn import p16, p64
import struct


class Assembler:
    def __init__(self):
        self.__code = []
        self.__labels = {}
        self.__constants = bytes()
        self.lookup = {}
        self.stdin = b''
        self.stdout = b''

    def add_bytes(self, b):
        assert isinstance(b, bytes)
        if b in self.lookup:
            return self.lookup[b]
        idx = len(self.__constants)
        self.__constants += b
        self.__constants += bytes([0])
        self.lookup[b] = idx
        return idx

    def add_number(self, n):
        idx = len(self.__constants)
        self.__constants += struct.pack('d', n)
        return idx

    def __bytes__(self):
        header = b"\x01LSP" + p16(len(self.__code)) + p16(len(self.__constants)) + p64(0x10)
        linked = b''.join([x if isinstance(x, bytes) else p16(self.__labels[x]) for x in self.__code])
        return header + linked + self.__constants

    def label(self, name):
        self.__labels[name] = len(self.__code)

    def funcall(self, numargs):
        self.__code += [p16(0x1 | (numargs << 8))]

    def jmp(self, f):
        self.__code += [p16(0x2), f]

    def cjmp(self, f, b):
        self.__code += [p16(0x3 | ((0 if b else 1) << 8)), f]

    def ret(self, n):
        self.__code += [p16(0x7 | (n << 8))]

    def defun(self, f, n):
        self.__code += [p16(0x8 | (n << 8)), f]

    def plus(self):
        self.__code += [p16(0x10)]

    def minus(self):
        self.__code += [p16(0x11)]

    def mul(self):
        self.__code += [p16(0x12)]

    def div(self):
        self.__code += [p16(0x13)]

    def car(self):
        self.__code += [p16(0x14)]

    def cdr(self):
        self.__code += [p16(0x15)]

    def logical_and(self):
        self.__code += [p16(0x16)]

    def logical_not(self):
        self.__code += [p16(0x17)]

    def logical_or(self):
        self.__code += [p16(0x18)]

    def eq(self):
        self.__code += [p16(0x19)]

    def numeq(self):
        self.__code += [p16(0x1a)]

    def numlt(self):
        self.__code += [p16(0x11a)]

    def numgt(self):
        self.__code += [p16(0x21a)]

    def set(self, idx):
        self.__code += [p16(0x1b | (idx << 8))]

    def setf(self, idx):
        self.__code += [p16(0x1c | (idx << 8))]

    def setq(self):
        self.__code += [p16(0x1d)]

    def var(self):
        self.__code += [p16(0x1e)]

    def format(self, n):
        self.__code += [p16(0x1f | (n << 8))]

    def gc(self):
        self.__code += [p16(0x20)]

    def id(self):
        self.__code += [p16(0x21)]

    def print(self):
        self.__code += [p16(0x22)]

    def read(self):
        self.__code += [p16(0x23)]

    def symbol(self, s):
        self.__code += [p16(0x50), p16(s)]

    def string(self, s):
        self.__code += [p16(0x51), p16(s)]

    def cons(self):
        self.__code += [p16(0x52)]

    def number(self, n):
        self.__code += [p16(0x53), p16(self.add_number(n))]

    def nil(self):
        self.__code += [p16(0x54)]

    def t(self):
        self.__code += [p16(0x55)]

    def symbolp(self):
        self.__code += [p16(0x60)]

    def stringp(self):
        self.__code += [p16(0x61)]

    def consp(self):
        self.__code += [p16(0x62)]

    def numberp(self):
        self.__code += [p16(0x63)]

    def make_symbol(self):
        self.__code += [p16(0x70)]

    def make_string(self):
        self.__code += [p16(0x71)]

    def make_cons(self):
        self.__code += [p16(0x72)]

    def make_number(self):
        self.__code += [p16(0x73)]

    def dup(self, n):
        self.__code += [p16(0x74 | (n << 8))]

    def pop(self, n):
        self.__code += [p16(0x75 | (n << 8))]

    def pushdown(self, n):
        self.__code += [p16(0x76 | (n << 8))]
