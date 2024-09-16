#ifndef COMPILER_H
#define COMPILER_H

#include <stdio.h>
#include <stdint.h>

struct relocation;
struct Trie;

struct constant_data {
	char *constants;
	uint16_t constants_size;

	struct Trie *root;
};

struct binding {
	char *name;	
	uint16_t offset;
};

struct compilation_info {
	uint16_t *code;
	uint16_t code_size;

	struct relocation *relocations;
	struct constant_data *constant_data;
	struct binding *bindings;
	size_t numbindings;
};

struct compilation_info compile(FILE *);

#endif
