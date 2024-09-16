#!/usr/bin/env python3

from pwn import *
import struct

STRING = p16(0x51)
CONS = p16(0x52)
NUMBER = p16(0x53)
NIL = p16(0x54)
T = p16(0x55)

CAR = p16(0x14)
CDR = p16(0x15)

MAKE_CONS = p16(0x72)

def RET(x):
    return p16(0x7 | (x << 8))
def SETF(x):
    return p16(0x1c | (x << 8))

def POP(x):
    return p16(0x75 | (x << 8))

def DUP(x):
    return p16(0x74 | (x << 8))

def FORMAT(x):
    return p16(0x1f | (x << 8))

NUMEQ = p16(0x1a)
GC = p16(0x20)
ID = p16(0x21)
PRINT = p16(0x22)
READ = p16(0x23)

code = READ + ID + GC + READ + ID + NUMEQ + STRING + p16(0) + FORMAT(1) + PRINT + RET(0)
constants = b"%s\x00"

header = b"\x01LSP" + p16(len(code) // 2) + p16(len(constants)) + p64(0x10)

with open('testgc.lispc', 'wb') as f:
    f.write(header + code + constants)


p = process(["../lvm/backend/lisp", "testgc.lispc"])

p.sendline(b'1337')
p.sendline(b'42')

p.interactive()
