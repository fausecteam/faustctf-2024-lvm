#include "vm.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/param.h>

#include "error.h"
#include "instruction.h"
#include "map.h"
#include "parser.h"

struct Function {
	uint16_t offset;
	uint8_t argcount;
};

struct Variant t = {.type = TYPE_SYMBOL, .data = {.symbol = {.name = "t", .constant = true}}};
struct Variant nil = {.type = TYPE_SYMBOL, .data = {.symbol = {.name = "nil", .constant = true}}};

struct type_info {
	bool checked;
	int *stack;
	size_t stackcount;
};

struct vm {
	uint16_t *code;
	char *constants;
	uint32_t constants_size;
	uint16_t *callstack;
	uint16_t *callstackend;
	struct Variant **datastack;
	size_t datacount;

	struct hashmap *functions;
	struct hashmap *variables;

	struct Variant **objects;
	size_t numobjects;
	size_t lastgc;

	uint16_t ip;

	struct type_info types[65536];
};

void get_bytes(void *dest, size_t size, size_t nmemb, FILE *f) {
	if (fread(dest, size, nmemb, f) != nmemb) {
		if (ferror(f)) {
			perror("fread");
		} else {
			fprintf(stderr, "Unexpected EOF\n");
		}
		exit(EXIT_FAILURE);
	}
}

struct vm *create_vm(FILE *f) {
	struct vm *vm = malloc(sizeof(struct vm));
	if (!vm) {
		perror("malloc");
		exit(EXIT_FAILURE);
	}
	vm->ip = 0;
	vm->objects = NULL;
	vm->numobjects = 0;
	vm->lastgc = 0;
	vm->functions = hashmap_new();
	vm->variables = hashmap_new();
	memset(vm->types, 0, sizeof(vm->types));
	vm->code = calloc(65536, sizeof(uint16_t));
	if (!vm->code) {
		perror("calloc");
		exit(EXIT_FAILURE);
	}
	vm->constants = calloc(65536, sizeof(uint16_t));
	if (!vm->constants) {
		perror("calloc");
		exit(EXIT_FAILURE);
	}
	struct bytecode_header header;
	get_bytes(&header, sizeof(header), 1, f);
	if (strncmp(header.magic, "LSP", 3) != 0) {
		fprintf(stderr, "Not a valid lisp bytecode file\n");
		exit(EXIT_FAILURE);
	}
	if (header.version != 1) {
		fprintf(stderr, "Incompatible bytecode version: %d\n", header.version);
		exit(EXIT_FAILURE);
	}
	if (fseek(f, header.fileoffset, SEEK_SET) < 0) {
		perror("fseek");
		exit(EXIT_FAILURE);
	}
	get_bytes(vm->code, sizeof(uint16_t), header.codelength, f);
	get_bytes(vm->constants, 1, header.constantlength, f);

	void *callstack = mmap((void *)0x1337000, 0x1000, PROT_READ | PROT_WRITE,
						   MAP_PRIVATE | MAP_ANONYMOUS | MAP_GROWSDOWN | MAP_FIXED, -1, 0);
	if (!callstack) {
		perror("mmap");
		exit(EXIT_FAILURE);
	}
	vm->callstack = vm->callstackend = callstack + 0x1000;
	void *datastack = mmap((void *)0x7331000, 0x1000, PROT_READ | PROT_WRITE,
						   MAP_PRIVATE | MAP_ANONYMOUS | MAP_GROWSDOWN | MAP_FIXED, -1, 0);
	if (!datastack) {
		perror("mmap");
		exit(EXIT_FAILURE);
	}
	vm->datastack = datastack + 0x1000;
	vm->datacount = 0;
	
	return vm;
}

static void delete_noop(union VariantData data) { (void)data; }

static void delete_string(union VariantData data) {
	if (!data.string.constant) free(data.string.value);
}

static void delete_symbol(union VariantData data) {
	if (!data.symbol.constant) free(data.symbol.name);
}

void (*deletion_function[9])(union VariantData) = {
	[TYPE_CONS] = delete_noop,
	[TYPE_NUMBER] = delete_noop,
	[TYPE_STRING] = delete_string,
	[TYPE_SYMBOL] = delete_symbol,
};

static void delete_variant(struct Variant *v) {
	deletion_function[v->type](v->data);
	free(v);
}

void delete_vm(struct vm *vm) {
	for (size_t i = 0; i < vm->numobjects; ++i) {
		delete_variant(vm->objects[i]);
	}
	for (size_t i = 0; i < 65536; ++i) {
		if (vm->types[i].checked)
			free(vm->types[i].stack);
	}
	hashmap_iter_values(vm->functions, free);
	hashmap_delete(vm->functions);
	hashmap_delete(vm->variables);
	free(vm->objects);
	free(vm->code);
	free(vm->constants);

	// This may unmap too much, but who cares ¯\_(ツ)_/¯
	// There should be nothing there besides our stacks
	munmap(NULL, 0x7332000);
	free(vm);
}

static void gc_visit(struct Variant *v) {
	v->active = true;

	switch (v->type) {
		case TYPE_CONS:
			gc_visit(v->data.cons.car);
			gc_visit(v->data.cons.cdr);
			break;
	}
}

void vm_gc(struct vm *vm) {
	// mark
	for (size_t i = 0; i < vm->numobjects; ++i) {
		vm->objects[i]->active = false;
	}

	for (size_t i = 0; i < vm->datacount; ++i) {
		gc_visit(vm->datastack[i]);
	}

	hashmap_iter_values(vm->variables, (void (*)(void *))gc_visit);

	// sweep
	size_t len = vm->numobjects;
	struct Variant **ptr = vm->objects;
	for (size_t i = 0; i < len; ++i) {
		if (vm->objects[i]->active) {
			*ptr = vm->objects[i];
			++ptr;
		} else {
			delete_variant(vm->objects[i]);
			--vm->numobjects;
		}
	}

	// possibly free up space
	vm->objects = reallocarray(vm->objects, vm->numobjects, sizeof(struct Variant *));
	if (!vm->objects && vm->numobjects) {
		perror("reallocarray");
		exit(EXIT_FAILURE);
	}
	vm->lastgc = 0;
}

static void vm_register(struct vm *vm, struct Variant *v) {
	++vm->lastgc;
	vm->objects = reallocarray(vm->objects, ++vm->numobjects, sizeof(struct Variant *));
	if (!vm->objects) {
		perror("realloc");
		exit(EXIT_FAILURE);
	}
	vm->objects[vm->numobjects - 1] = v;
}

struct Variant *variant_new(struct vm *vm, int type) {
	struct Variant *v = malloc(sizeof(struct Variant));
	if (!v) {
		perror("variant_new");
		exit(EXIT_FAILURE);
	}
	v->type = type;
	vm_register(vm, v);
	return v;
}

uint16_t vm_next_opcode(struct vm *vm) { return vm->code[vm->ip++]; }
uint16_t vm_get_ip(struct vm *vm) { return vm->ip; }

char *vm_get_string(struct vm *vm, uint16_t address) { return vm->constants + address; }

double vm_get_number(struct vm *vm, uint16_t address) {
	double res;
	memcpy(&res, vm->constants + address, sizeof(double));
	return res;
}

static struct Variant *data_pop(struct vm *vm) {
	struct Variant *ret = *vm->datastack;
	++vm->datastack;
	--vm->datacount;
	return ret;
}

static void data_push(struct vm *vm, struct Variant *v) {
	--vm->datastack;
	++vm->datacount;
	*vm->datastack = v;
}

static int *dup_array(int *array, size_t len) {
	int *ret = calloc(sizeof(int), len);
	if (!ret) {
		perror("calloc");
		exit(EXIT_FAILURE);
	}
	memcpy(ret, array, sizeof(int) * len);
	return ret;
}

static char *get_type(int type) {
	switch (type) {
		case TYPE_SYMBOL:
			return "Symbol";
		case TYPE_STRING:
			return "String";
		case TYPE_CONS:
			return "Cons";
		case TYPE_NUMBER:
			return "Number";
		case TYPE_UNKNWN:
			return "Unknown Type";
		default:
			return "[BUG]";
	}
}

static int *vm_check(struct vm *vm, uint16_t entry, int *stack, size_t stackcount) {
	uint16_t ip = entry;
	if (vm->types[ip].checked) {
		if (stackcount != vm->types[ip].stackcount) {
			fprintf(stderr, "Stack differs for callers (length) @0x%04x: %zu:%zu\n", ip, vm->types[ip].stackcount,
					stackcount);
			exit(EXIT_FAILURE);
		}
		bool suitable = true;
		for (size_t i = 0; i < stackcount; ++i) {
			if (vm->types[ip].stack[i] != stack[i] && vm->types[ip].stack[i] != TYPE_UNKNWN) {
				// types differ and we have to check again with looser type information
				stack[i] = TYPE_UNKNWN;
				suitable = false;
			}
		}

		if (suitable)
			// We already checked this block and the caller stack is suitable
			return stack;

		free(vm->types[ip].stack);
	}
	vm->types[ip].checked = true;
	vm->types[ip].stack = dup_array(stack, stackcount);
	vm->types[ip].stackcount = stackcount;

#define CHECK_STACK(count)                                         \
	if (stackcount < (count)) {                                    \
		fprintf(stderr, "Not enough data on stack @0x%04x\n", ip); \
		exit(EXIT_FAILURE);                                        \
	}

#define GET(idx) (stack[stackcount - 1 - idx])
#define PUSH(TYPE)                                            \
	do {                                                      \
		int __type = TYPE;                                    \
		++stackcount;                                         \
		stack = reallocarray(stack, sizeof(int), stackcount); \
		if (!stack) {                                         \
			perror("reallocarray");                           \
			exit(EXIT_FAILURE);                               \
		}                                                     \
		stack[stackcount - 1] = __type;                       \
	} while (0)
#define POP(TYPE)                                                                               \
	do {                                                                                        \
		int __type = stack[stackcount - 1];                                                     \
		if ((__type & (TYPE)) != __type) {                                                      \
			fprintf(stderr, "Does not typecheck %s != %s @%x\n", get_type(__type), get_type(TYPE), ip); \
			exit(EXIT_FAILURE);                                                                 \
		}                                                                                       \
		--stackcount;                                                                           \
	} while (0)

	vm->ip = ip;

	while (42) {
		ip = vm_get_ip(vm);
		struct instruction ins = parse_instruction(vm);
		uint16_t nip = vm_get_ip(vm);
		switch (ins.type) {
			case INSTRUCTION_JMP:
				return vm_check(vm, ins.address, stack, stackcount);
			case INSTRUCTION_CJMP:
				CHECK_STACK(1);
				POP(TYPE_ANY);
				free(vm_check(vm, ins.address, dup_array(stack, stackcount), stackcount));
				vm->ip = nip;
				break;
			case INSTRUCTION_FUNCALL:
				CHECK_STACK(ins.numargs + 1);
				POP(TYPE_SYMBOL);
				stackcount -= ins.numargs;
				PUSH(TYPE_UNKNWN);
				break;
			case INSTRUCTION_DEFUN: {
				CHECK_STACK(1);
				POP(TYPE_SYMBOL);
				int *newstack = calloc(sizeof(int), ins.numargs);
				if (!newstack) {
					perror("calloc");
					exit(EXIT_FAILURE);
				}
				for (size_t i = 0; i < ins.numargs; ++i) {
					newstack[i] = TYPE_UNKNWN;
				}
				free(vm_check(vm, ins.address, newstack, ins.numargs));
				vm->ip = nip;
				PUSH(TYPE_SYMBOL);
				break;
			}
			case INSTRUCTION_RET:
				if (stackcount != ins.stackref + 1) {
					fprintf(stderr, "Function @0x%04x does not clean up stack properly %zu %zu\n", entry, stackcount, ins.stackref + 1);
					exit(EXIT_FAILURE);
				}
				return stack;
			case INSTRUCTION_PLUS:
			case INSTRUCTION_MINUS:
			case INSTRUCTION_MUL:
			case INSTRUCTION_DIV:
				CHECK_STACK(2);
				POP(TYPE_NUMBER);
				POP(TYPE_NUMBER);
				PUSH(TYPE_NUMBER);
				break;
			case INSTRUCTION_CAR:
			case INSTRUCTION_CDR:
				CHECK_STACK(1);
				POP(TYPE_CONS);
				PUSH(TYPE_UNKNWN);
				break;
			case INSTRUCTION_ID:
				CHECK_STACK(1);
				POP(TYPE_ANY);
				PUSH(TYPE_NUMBER);
				break;
			case INSTRUCTION_AND:
				CHECK_STACK(2);
				POP(TYPE_ANY);
				POP(TYPE_ANY);
				PUSH(TYPE_UNKNWN);
				break;
			case INSTRUCTION_NOT:
				CHECK_STACK(1);
				POP(TYPE_ANY);
				PUSH(TYPE_SYMBOL);
				break;
			case INSTRUCTION_OR:
				CHECK_STACK(2);
				POP(TYPE_ANY);
				POP(TYPE_ANY);
				PUSH(TYPE_UNKNWN);
				break;
			case INSTRUCTION_EQ:
				CHECK_STACK(2);
				POP(TYPE_ANY);
				POP(TYPE_ANY);
				PUSH(TYPE_SYMBOL);
				break;
			case INSTRUCTION_NUMCMP:
				CHECK_STACK(2);
				POP(TYPE_NUMBER);
				POP(TYPE_NUMBER);
				PUSH(TYPE_SYMBOL);
				break;
			case INSTRUCTION_VAR:
				CHECK_STACK(1);
				POP(TYPE_SYMBOL);
				PUSH(TYPE_UNKNWN);
				break;
			case INSTRUCTION_SETQ:
				CHECK_STACK(2);
				POP(TYPE_SYMBOL);
				break;
			case INSTRUCTION_SYMBOL:
				PUSH(TYPE_SYMBOL);
				break;
			case INSTRUCTION_STRING:
				PUSH(TYPE_STRING);
				break;
			case INSTRUCTION_CONS:
				CHECK_STACK(2);
				POP(TYPE_ANY);
				POP(TYPE_ANY);
				PUSH(TYPE_CONS);
				break;
			case INSTRUCTION_NUMBER:
				PUSH(TYPE_NUMBER);
				break;
			case INSTRUCTION_NIL:
				PUSH(TYPE_SYMBOL);
				break;
			case INSTRUCTION_T:
				PUSH(TYPE_SYMBOL);
				break;
			case INSTRUCTION_SYMBOLP:
			case INSTRUCTION_STRINGP:
			case INSTRUCTION_CONSP:
			case INSTRUCTION_NUMBERP:
				CHECK_STACK(1);
				POP(TYPE_UNKNWN);
				PUSH(TYPE_SYMBOL);
				break;
			case INSTRUCTION_MAKE_SYMBOL:
				CHECK_STACK(1);
				POP(TYPE_ANY);
				PUSH(TYPE_SYMBOL);
				break;
			case INSTRUCTION_MAKE_STRING:
				CHECK_STACK(1);
				POP(TYPE_ANY);
				PUSH(TYPE_STRING);
				break;
			case INSTRUCTION_MAKE_CONS:
				CHECK_STACK(1);
				POP(TYPE_ANY);
				PUSH(TYPE_CONS);
				break;
			case INSTRUCTION_MAKE_NUMBER:
				CHECK_STACK(1);
				POP(TYPE_ANY);
				PUSH(TYPE_NUMBER);
				break;
			case INSTRUCTION_DUP: {
				CHECK_STACK(ins.stackref + 1);
				PUSH(GET(ins.stackref));
				break;
			}
			case INSTRUCTION_GC:
				break;
			case INSTRUCTION_SET:
				CHECK_STACK(ins.stackref + 1);
				stack[stackcount - 2 - ins.stackref] = stack[stackcount - 1];
				break;
			case INSTRUCTION_SETF:
				CHECK_STACK(ins.stackref + 1);
				stack[stackcount - 2 - ins.stackref] = stack[stackcount - 1];
				break;
			case INSTRUCTION_FORMAT:
				CHECK_STACK(ins.stackref + 1);
				POP(TYPE_STRING);
				for (uint8_t i = 0; i < ins.stackref; ++i) POP(TYPE_ANY);
				PUSH(TYPE_STRING);
				break;
			case INSTRUCTION_PRINT:
				CHECK_STACK(1);
				POP(TYPE_STRING);
				PUSH(TYPE_STRING);
				break;
			case INSTRUCTION_READ:
				PUSH(TYPE_UNKNWN);
				break;
			case INSTRUCTION_POP:
				CHECK_STACK(ins.stackref);
				for (uint8_t i = 0; i < ins.stackref; ++i) POP(TYPE_ANY);
				break;
			case INSTRUCTION_PUSHDOWN: {
				CHECK_STACK(ins.stackref + 1);
				int type = GET(0);
				for (uint8_t i = 0; i < ins.stackref + 1; ++i) POP(TYPE_ANY);
				PUSH(type);
				break;
			}
			default:
				fprintf(stderr, "Invalid instruction @0x%04x\n", ip);
				exit(EXIT_FAILURE);
		}
	}
	return stack;
}

static size_t format_variant(char *output, size_t size, struct Variant *val) {
	size_t len = 0;
	switch (val->type) {
		case TYPE_STRING: {
			size_t l = strlen(val->data.string.value);
			if (size > 0) memcpy(output, val->data.string.value, MIN(size, l));
			output += l;
			len += l;
			break;
		}
		case TYPE_SYMBOL: {
			size_t l = strlen(val->data.symbol.name);
			if (size > 0) memcpy(output, val->data.symbol.name, MIN(size, l));
			output += l;
			len += l;
			break;
		}
		case TYPE_NUMBER: {
			size_t l = snprintf(output, size, "%lf", val->data.number);
			output += l;
			len += l;
			break;
		}
		case TYPE_CONS: {
#define DO_PRINTC(c)                   \
	do {                               \
		if (size > len) *output = (c); \
		++output;                      \
		++len;                         \
	} while (0);

			DO_PRINTC('(');

			do {
				size_t l = format_variant(output, size > len ? size - len : 0, val->data.cons.car);
				len += l;
				output += l;
				val = val->data.cons.cdr;
				if (val != &nil)
					DO_PRINTC(' ');
			} while (val->type == TYPE_CONS);

			if (val != &nil) {
				DO_PRINTC('.');
				DO_PRINTC(' ');
				
				size_t l = format_variant(output, size > len ? size - len : 0, val);
				len += l;
				output += l;
			}

			DO_PRINTC(')');
			break;
#undef DO_PRINTC
		}
	}

	return len;
}

static size_t lisp_format(char *output, size_t size, char *fmt, struct Variant **data, size_t count) {
	size_t argi = 0;
	size_t len = 0;
	while (*fmt) {
		switch (*fmt) {
			case '%':
				++fmt;
				switch (*fmt) {
					case 's': {
						if (argi >= count) {
							fprintf(stderr, "Not enough format string arguments\n");
							exit(EXIT_FAILURE);
						}
						size_t l = format_variant(output, size > len ? size - len : 0, data[argi]);
						output += l;
						len += l;
						++argi;
						break;
					}
					case 'd': {
						if (argi >= count) {
							fprintf(stderr, "Not enough format string arguments\n");
							exit(EXIT_FAILURE);
						}
						if (data[argi]->type != TYPE_NUMBER) {
							fprintf(stderr, "Format string argument doesn't match type\n");
							exit(EXIT_FAILURE);
						}
						size_t l = snprintf(output, size > len ? size - len : 0, "%ld", (size_t)data[argi]->data.number);
						output += l;
						len += l;
						++argi;
						break;
					}
					case 'x': {
						if (argi >= count) {
							fprintf(stderr, "Not enough format string arguments\n");
							exit(EXIT_FAILURE);
						}
						if (data[argi]->type != TYPE_NUMBER) {
							fprintf(stderr, "Format string argument doesn't match type\n");
							exit(EXIT_FAILURE);
						}
						size_t l = snprintf(output, size > len ? size - len : 0, "%lx", (size_t)data[argi]->data.number);
						output += l;
						len += l;
						++argi;
						break;
					}
					case 'f': {
						if (argi >= count) {
							fprintf(stderr, "Not enough format string arguments\n");
							exit(EXIT_FAILURE);
						}
						if (data[argi]->type != TYPE_NUMBER) {
							fprintf(stderr, "Format string argument doesn't match type\n");
							exit(EXIT_FAILURE);
						}
						size_t l = snprintf(output, size > len ? size - len : 0, "%lf", data[argi]->data.number);
						output += l;
						len += l;
						++argi;
						break;
					}
					case '%':
						++len;
						if (size > len) {
							*output = '%';
						}
						++output;
						break;
					default:
						fprintf(stderr, "Invalid format string\n");
						exit(EXIT_FAILURE);
				}
				break;
			default:
				++len;
				if (output && size >= len) {
					*output = *fmt;
					++output;
				}
		}
		++fmt;
	}

	if (size > len) {
		*output = '\0';
	}
	return len;
}

struct Variant *vm_run(struct vm *vm) {
	// type checking of instructions
	free(vm_check(vm, vm->ip, NULL, 0));
	vm->ip = 0;

	while (42) {
		uint16_t ip = vm->ip;
		struct instruction ins = parse_instruction(vm);
		if (vm->lastgc > 1024) vm_gc(vm);
		switch (ins.type) {
			case INSTRUCTION_JMP:
				vm->ip = ins.address;
				break;
			case INSTRUCTION_CJMP: {
				struct Variant *condition = data_pop(vm);
				if (!!(condition == &nil) == !!ins.cond) vm->ip = ins.address;
				break;
			}
			case INSTRUCTION_FUNCALL: {
				struct Variant *name = data_pop(vm);
				struct Function *f = hashmap_get(vm->functions, name->data.symbol.name);
				if (!f) {
					fprintf(stderr, "Function is void: %s\n", name->data.symbol.name);
					exit(EXIT_FAILURE);
				}
				if (f->argcount != ins.numargs) {
					fprintf(stderr, "Argument mismatch for %s: expected %d got %zu\n", name->data.string.value, f->argcount, ins.numargs);
					exit(EXIT_FAILURE);
				}
				--vm->callstack;
				*vm->callstack = vm->ip;

				vm->ip = f->offset;
				break;
			}
			case INSTRUCTION_DEFUN: {
				struct Variant *name = data_pop(vm);
				struct Function *f = malloc(sizeof(struct Function));
				if (!f) {
					perror("malloc");
					exit(EXIT_FAILURE);
				}
				f->offset = ins.address;
				f->argcount = ins.numargs;
				hashmap_insert(vm->functions, name->data.symbol.name, f);
				data_push(vm, name);
				break;
			}
			case INSTRUCTION_RET: {
				// exit condition
				struct Variant *res = data_pop(vm);
				if (vm->callstack == vm->callstackend) return res;
				vm->ip = *vm->callstack;
				++vm->callstack;
				vm->datacount -= ins.stackref;
				vm->datastack += ins.stackref;
				data_push(vm, res);
				break;
			}
			case INSTRUCTION_PLUS: {
				double a = data_pop(vm)->data.number;
				double b = data_pop(vm)->data.number;
				struct Variant *v = variant_new(vm, TYPE_NUMBER);
				v->data.number = a + b;
				data_push(vm, v);
				break;
			}
			case INSTRUCTION_MINUS: {
				double a = data_pop(vm)->data.number;
				double b = data_pop(vm)->data.number;
				struct Variant *v = variant_new(vm, TYPE_NUMBER);
				v->data.number = a - b;
				data_push(vm, v);
				break;
			}
			case INSTRUCTION_MUL: {
				double a = data_pop(vm)->data.number;
				double b = data_pop(vm)->data.number;
				struct Variant *v = variant_new(vm, TYPE_NUMBER);
				v->data.number = a * b;
				data_push(vm, v);
				break;
			}
			case INSTRUCTION_DIV: {
				double a = data_pop(vm)->data.number;
				double b = data_pop(vm)->data.number;
				struct Variant *v = variant_new(vm, TYPE_NUMBER);
				v->data.number = a / b;
				data_push(vm, v);
				break;
			}
			case INSTRUCTION_CAR: {
				data_push(vm, data_pop(vm)->data.cons.car);
				break;
			}
			case INSTRUCTION_CDR: {
				data_push(vm, data_pop(vm)->data.cons.cdr);
				break;
			}
			case INSTRUCTION_AND: {
				struct Variant *a = data_pop(vm);
				struct Variant *b = data_pop(vm);
				if (a == &nil)
					data_push(vm, a);
				else
					data_push(vm, b);
				break;
			}
			case INSTRUCTION_NOT: {
				struct Variant *data = data_pop(vm);
				if (data == &nil)
					data_push(vm, &t);
				else
					data_push(vm, &nil);
				break;
			}
			case INSTRUCTION_OR: {
				struct Variant *a = data_pop(vm);
				struct Variant *b = data_pop(vm);
				if (a == &nil)
					data_push(vm, b);
				else
					data_push(vm, a);
				break;
			}
			case INSTRUCTION_EQ: {
				struct Variant *a = data_pop(vm);
				struct Variant *b = data_pop(vm);
				if (a == b || (a->type == TYPE_SYMBOL && b->type == TYPE_SYMBOL && a->data.symbol.name == b->data.symbol.name))
					data_push(vm, &t);
				else
					data_push(vm, &nil);
			} break;
			case INSTRUCTION_NUMCMP: {
				double a = data_pop(vm)->data.number;
				double b = data_pop(vm)->data.number;
				switch (ins.cond) {
					case 0:
						data_push(vm, a == b ? &t : &nil);
						break;
					case 1:
						data_push(vm, a < b ? &t : &nil);
						break;
					case 2:
						data_push(vm, a > b ? &t : &nil);
						break;
				}
			} break;
			case INSTRUCTION_VAR: {
				struct Variant *name = data_pop(vm);
				struct Variant *var = hashmap_get(vm->variables, name->data.symbol.name);
				if (!var) {
					fprintf(stderr, "Variable is void: %s\n", name->data.symbol.name);
					exit(EXIT_FAILURE);
				}
				data_push(vm, var);
				break;
			}
			case INSTRUCTION_SETQ: {
				struct Variant *name = data_pop(vm);
				struct Variant *value = data_pop(vm);
				hashmap_insert(vm->variables, name->data.symbol.name, value);
				data_push(vm, value);
			} break;
			case INSTRUCTION_SYMBOL: {
				struct Variant *v = variant_new(vm, TYPE_SYMBOL);
				v->data.symbol.constant = true;
				v->data.symbol.name = vm_get_string(vm, ins.address);
				data_push(vm, v);
				break;
			}
			case INSTRUCTION_STRING: {
				struct Variant *v = variant_new(vm, TYPE_STRING);
				v->data.string.constant = true;
				v->data.string.value = vm_get_string(vm, ins.address);
				data_push(vm, v);
				break;
			}
			case INSTRUCTION_CONS: {
				struct Variant *v = variant_new(vm, TYPE_CONS);
				v->data.cons.car = data_pop(vm);
				v->data.cons.cdr = data_pop(vm);
				data_push(vm, v);
				break;
			}
			case INSTRUCTION_NUMBER: {
				struct Variant *v = variant_new(vm, TYPE_NUMBER);
				v->data.number = vm_get_number(vm, ins.address);
				data_push(vm, v);
				break;
			}
			case INSTRUCTION_NIL: {
				data_push(vm, &nil);
				break;
			}
			case INSTRUCTION_T: {
				data_push(vm, &t);
				break;
			}
			case INSTRUCTION_SYMBOLP:
				if (data_pop(vm)->type == TYPE_SYMBOL)
					data_push(vm, &t);
				else
					data_push(vm, &nil);
				break;
			case INSTRUCTION_STRINGP:
				if (data_pop(vm)->type == TYPE_STRING)
					data_push(vm, &t);
				else
					data_push(vm, &nil);
				break;
			case INSTRUCTION_CONSP:
				if (data_pop(vm)->type == TYPE_CONS)
					data_push(vm, &t);
				else
					data_push(vm, &nil);
				break;
			case INSTRUCTION_NUMBERP:
				if (data_pop(vm)->type == TYPE_NUMBER)
					data_push(vm, &t);
				else
					data_push(vm, &nil);
				break;
			case INSTRUCTION_MAKE_SYMBOL:
				if (vm->datastack[0]->type != TYPE_SYMBOL) {
					fprintf(stderr, "Symbol expected, got %s @0x%04x\n", get_type(vm->datastack[0]->type), vm->ip);
					exit(EXIT_FAILURE);
				}
				break;
			case INSTRUCTION_MAKE_STRING:
				if (vm->datastack[0]->type != TYPE_STRING) {
					fprintf(stderr, "String expected, got %s @0x%04x\n", get_type(vm->datastack[0]->type), vm->ip);
					exit(EXIT_FAILURE);
				}
				break;
			case INSTRUCTION_MAKE_CONS:
				if (vm->datastack[0]->type != TYPE_CONS) {
					fprintf(stderr, "Cons-cell expected, got %s @0x%04x\n", get_type(vm->datastack[0]->type), vm->ip);
					exit(EXIT_FAILURE);
				}
				break;
			case INSTRUCTION_MAKE_NUMBER:
				if (vm->datastack[0]->type != TYPE_NUMBER) {
					fprintf(stderr, "Number expected, got %s @0x%04x\n", get_type(vm->datastack[0]->type), vm->ip);
					exit(EXIT_FAILURE);
				}
				break;
			case INSTRUCTION_DUP:
				data_push(vm, vm->datastack[ins.stackref]);
				break;
			case INSTRUCTION_GC:
				vm_gc(vm);
				break;
			case INSTRUCTION_ID: {
				struct Variant *v = data_pop(vm);
				double id = (uintptr_t)v;
				struct Variant *res = variant_new(vm, TYPE_NUMBER);
				res->data.number = id;
				data_push(vm, res);
				break;
			}
			case INSTRUCTION_SET: {
				struct Variant *val = data_pop(vm);
				vm->datastack[ins.stackref] = val;
				data_push(vm, val);
				break;
			}
			case INSTRUCTION_SETF: {
				struct Variant *val = data_pop(vm);
				*vm->datastack[ins.stackref] = *val;
				data_push(vm, vm->datastack[ins.stackref]);
				break;
			}
			case INSTRUCTION_FORMAT: {
				struct Variant *val = data_pop(vm);
				char *fmt = val->data.string.value;
				size_t len = lisp_format(NULL, 0, fmt, vm->datastack, ins.stackref);
				char *res = malloc(len + 1);
				if (!res) {
					perror("malloc");
					exit(EXIT_FAILURE);
				}
				lisp_format(res, len + 1, fmt, vm->datastack, ins.stackref);
				for (uint8_t i = 0; i < ins.stackref; ++i) data_pop(vm);
				struct Variant *v = variant_new(vm, TYPE_STRING);
				v->data.string.constant = false;
				v->data.string.value = res;
				data_push(vm, v);
				break;
			}
			case INSTRUCTION_PRINT: {
				struct Variant *val = data_pop(vm);
				puts(val->data.string.value);
				data_push(vm, val);
				break;
			}
			case INSTRUCTION_READ: {
				struct Variant *v = parse_sexp(vm, stdin);
				if (v) {
					data_push(vm, v);
				} else {
					if (!parsing_error && !feof(stdin))
						fprintf(stderr, "Malformed input\n");
					data_push(vm, &nil);
				}
				break;
			}
			case INSTRUCTION_POP:
				for (uint8_t i = 0; i < ins.stackref; ++i) data_pop(vm);
				break;
			case INSTRUCTION_PUSHDOWN: {
				struct Variant *ret = data_pop(vm);
				for (uint8_t i = 0; i < ins.stackref; ++i) data_pop(vm);
				data_push(vm, ret);
				break;
			}
			default:
				fprintf(stderr, "[BUG] Invalid instruction @0x%04x\n", ip);
				exit(EXIT_FAILURE);
		}
	}
}

void print_variant(struct Variant *v) {
	switch (v->type) {
	case TYPE_SYMBOL:
		printf("%s", v->data.symbol.name);
		break;
	case TYPE_STRING:
		printf("\"%s\"", v->data.string.value);
		break;
	case TYPE_NUMBER:
		printf("%lf", v->data.number);
		break;
	case TYPE_CONS:
		putchar('(');
		while (v->type == TYPE_CONS) {
			print_variant(v->data.cons.car);
			v = v->data.cons.cdr;
			if (v != &nil)
				putchar(32);
		}
		if (v != &nil) {
			printf(". ");
			print_variant(v);
		}
		putchar(')');
		break;
	}
}
