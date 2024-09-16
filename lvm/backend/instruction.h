#ifndef INSTRUCTION_H
#define INSTRUCTION_H

#include <stddef.h>
#include <stdint.h>

#define UNDEFINED_INSTRUCTION -1

#define INSTRUCTION_FUNCALL 0x1
#define INSTRUCTION_JMP 0x2
#define INSTRUCTION_CJMP 0x3
#define INSTRUCTION_RET 0x7
#define INSTRUCTION_DEFUN 0x8

#define INSTRUCTION_PLUS 0x10
#define INSTRUCTION_MINUS 0x11
#define INSTRUCTION_MUL 0x12
#define INSTRUCTION_DIV 0x13
#define INSTRUCTION_CAR 0x14
#define INSTRUCTION_CDR 0x15
#define INSTRUCTION_AND 0x16
#define INSTRUCTION_NOT 0x17
#define INSTRUCTION_OR 0x18
#define INSTRUCTION_EQ 0x19
#define INSTRUCTION_NUMCMP 0x1a
#define INSTRUCTION_SET 0x1b
#define INSTRUCTION_SETF 0x1c
#define INSTRUCTION_SETQ 0x1d
#define INSTRUCTION_VAR 0x1e
#define INSTRUCTION_FORMAT 0x1f
#define INSTRUCTION_GC 0x20
#define INSTRUCTION_ID 0x21
#define INSTRUCTION_PRINT 0x22
#define INSTRUCTION_READ 0x23

#define INSTRUCTION_SYMBOL 0x50
#define INSTRUCTION_STRING 0x51
#define INSTRUCTION_CONS 0x52
#define INSTRUCTION_NUMBER 0x53
#define INSTRUCTION_NIL 0x54
#define INSTRUCTION_T 0x55

#define INSTRUCTION_SYMBOLP 0x60
#define INSTRUCTION_STRINGP 0x61
#define INSTRUCTION_CONSP 0x62
#define INSTRUCTION_NUMBERP 0x63

#define INSTRUCTION_MAKE_SYMBOL 0x70
#define INSTRUCTION_MAKE_STRING 0x71
#define INSTRUCTION_MAKE_CONS 0x72
#define INSTRUCTION_MAKE_NUMBER 0x73
#define INSTRUCTION_DUP 0x74
#define INSTRUCTION_POP 0x75
#define INSTRUCTION_PUSHDOWN 0x76

struct instruction {
	int type;
	union {
		struct {
			uint16_t address;
			union {
				size_t numargs;
				uint8_t cond;
			};
		};
		size_t stackref;
	};
};
struct vm;

struct instruction parse_instruction(struct vm *);

void print_instruction(struct vm *, struct instruction *);

#endif
