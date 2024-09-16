#ifndef VM_H
#define VM_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#define TYPE_UNKNWN (1 << 30)
#define TYPE_SYMBOL (1 << 0)
#define TYPE_CONS (1 << 1)
#define TYPE_NUMBER (1 << 2)
#define TYPE_STRING (1 << 3)

#define TYPE_ANY (TYPE_UNKNWN | TYPE_SYMBOL | TYPE_CONS | TYPE_NUMBER | TYPE_STRING)

struct Variant;

struct Symbol {
	char *name;
	bool constant;
};

struct String {
	char *value;
	bool constant;
};

struct Cons {
	struct Variant *car;
	struct Variant *cdr;
};

union VariantData {
	struct Symbol symbol;
	struct Cons cons;
	double number;
	struct String string;
};

struct Variant {
	bool active;
	int type;

	union VariantData data;
};

extern struct Variant t;
extern struct Variant nil;

struct vm;

struct bytecode_header {
	uint8_t version;
	char magic[3];
	uint16_t codelength;
	uint16_t constantlength;
	uint64_t fileoffset;
} __attribute__((packed));

struct vm *create_vm(FILE *);
void delete_vm(struct vm *);

uint16_t vm_next_opcode(struct vm *);
uint16_t vm_get_ip(struct vm *);

char *vm_get_string(struct vm *, uint16_t);
double vm_get_number(struct vm *, uint16_t);

struct Variant *variant_new(struct vm *, int);
struct Variant *vm_run(struct vm *);

void print_variant(struct Variant *);

#endif
