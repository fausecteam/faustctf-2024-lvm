#include "compiler.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "instruction.h"
#include "vm.h"
#include "parser.h"

struct relocation {
	struct relocation *next;

	bool iscompiled;

	uint16_t offset;
	union {
		struct {
			struct relocation *nested;
			uint16_t *code;
			uint16_t code_size;
		} compiled;
		struct {
			uint16_t jmptarget;
		} reference;
	} data;
};

struct Trie {
	struct Trie *next[256];
	uint16_t offset;
};

struct Variant *variant_new(struct vm *vm, int type) {
	// we have no gc, need to clean up manually
	(void) vm;
	
	struct Variant *v = malloc(sizeof(struct Variant));
	if (!v) {
		perror("malloc");
		exit(EXIT_FAILURE);
	}
	v->type = type;

	return v;
}

struct Variant nil = {.type = TYPE_SYMBOL, .data={.symbol = {.name = "nil"}}};

static void delete_variant(struct Variant *variant) {
	if (variant == &nil) {
		// this one doesn't need to be freed()
		return;
	}

	switch (variant->type) {
		case TYPE_SYMBOL:
			if (!variant->data.symbol.constant)
				free(variant->data.symbol.name);
			break;
		case TYPE_STRING:
			if (!variant->data.string.constant)
				free(variant->data.string.value);
			break;
		case TYPE_CONS:
			delete_variant(variant->data.cons.car);
			delete_variant(variant->data.cons.cdr);
			break;
	}

	free(variant);
}

static uint16_t add_instruction(struct compilation_info *info, uint16_t instruction) {
	++info->code_size;
	info->code = reallocarray(info->code, sizeof(uint16_t), info->code_size);

	if (!info->code) {
		perror("reallocarray");
		exit(EXIT_FAILURE);
	}

	info->code[info->code_size - 1] = instruction;
	return info->code_size - 1;
}

static uint16_t add_bytes(struct constant_data *data, void *buffer, size_t size) {
	uint16_t orig_size = data->constants_size;
	data->constants_size += size;
	data->constants = realloc(data->constants, data->constants_size);

	if (!data->constants) {
		perror("realloc");
		exit(EXIT_FAILURE);
	}

	memcpy(data->constants + orig_size, buffer, size);
	return orig_size;
}

static uint16_t add_number(struct compilation_info *info, double number) {
	return add_bytes(info->constant_data, &number, sizeof(double));
}

static uint16_t add_string(struct compilation_info *info, char *start) {
	char *end = start;
	while (*end) {
		++end;
	}

	struct Trie *t = info->constant_data->root;
	if (!t) {
		t = calloc(sizeof(struct Trie), 1);
		if (!t) {
			perror("malloc");
			exit(EXIT_FAILURE);
		}
		info->constant_data->root = t;
	}

	unsigned char *cur = (unsigned char *)end;
	while (cur >= (unsigned char *)start) {
		struct Trie *n = t->next[*cur];
		if (!n) {
			// This substring is not already present
			// allocate a new constant
			n = calloc(sizeof(struct Trie), 1);
			if (!n) {
				perror("calloc");
				exit(EXIT_FAILURE);
			}
			t->next[*cur] = n;
			n->offset = add_bytes(info->constant_data, start, end - start + 1) + (cur - (unsigned char *)start);
			--cur;
			t = n;
			break;
		}
		t = n;
		--cur;
	}

	// insert remaining chars
	while (cur >= (unsigned char *)start) {
		assert(!t->next[*cur]);
		t->next[*cur] = calloc(sizeof(struct Trie), 1);
		if (!t->next[*cur]) {
			perror("malloc");
			exit(EXIT_FAILURE);
		}
		t->next[*cur]->offset = t->offset - 1;
		t = t->next[*cur];
		--cur;
	}

	return t->offset;
}

static void add_binding(struct compilation_info *info, char *name, uint16_t offset) {
	++info->numbindings;
	info->bindings = reallocarray(info->bindings, sizeof(struct binding), info->numbindings);
	if (!info->bindings) {
		perror("reallocarray");
		exit(EXIT_FAILURE);
	}

	info->bindings[info->numbindings - 1].name = strdup(name);
	info->bindings[info->numbindings - 1].offset = offset;
}

static bool has_binding(struct compilation_info *info, char *name) {
	for (size_t i = info->numbindings; i-- > 0;) {
		if (strcmp(info->bindings[i].name, name) == 0) return true;
	}
	return false;
}

static uint16_t get_binding(struct compilation_info *info, char *name) {
	for (size_t i = info->numbindings; i-- > 0;) {
		if (strcmp(info->bindings[i].name, name) == 0) return info->bindings[i].offset;
	}
	assert(false);
}

static void remove_binding(struct compilation_info *info, char *name) {
	for (size_t i = info->numbindings; i-- > 0;) {
		if (strcmp(info->bindings[i].name, name) == 0) {
			free(info->bindings[i].name);
			--info->numbindings;
			for (size_t j = i; j < info->numbindings; ++j) {
				info->bindings[j] = info->bindings[j + 1];
			}
			return;
		}
	}
	assert(false);
}

static void delete_bindings(struct compilation_info *info) {
	for (size_t i = 0; i < info->numbindings; ++i) {
		free(info->bindings[i].name);
	}
	free(info->bindings);
	info->bindings = NULL;
	info->numbindings = 0;
}

static uint16_t len(struct Variant *variant) {
	uint16_t length = 0;
	while (variant != &nil) {
		assert(variant->type == TYPE_CONS);
		length += 1;
		variant = variant->data.cons.cdr;
	}
	return length;
}

static void compile_sexp(struct Variant *variant, struct compilation_info *info, size_t *stacklen);

static void compile_application(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	if (variant == &nil) return;
	assert(variant->type == TYPE_CONS);
	compile_application(variant->data.cons.cdr, info, stacklen);
	compile_sexp(variant->data.cons.car, info, stacklen);
}

static void insert_literal(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	switch (variant->type) {
		case TYPE_NUMBER: {
			uint16_t off = add_number(info, variant->data.number);
			add_instruction(info, INSTRUCTION_NUMBER);
			add_instruction(info, off);
			++*stacklen;
			break;
		}
		case TYPE_STRING: {
			uint16_t off = add_string(info, variant->data.string.value);
			add_instruction(info, INSTRUCTION_STRING);
			add_instruction(info, off);
			++*stacklen;
			break;
		}
		case TYPE_SYMBOL: {
			if (strcmp(variant->data.symbol.name, "nil") == 0) {
				add_instruction(info, INSTRUCTION_NIL);
			} else if (strcmp(variant->data.symbol.name, "t") == 0) {
				add_instruction(info, INSTRUCTION_T);
			} else {
				uint16_t off = add_string(info, variant->data.symbol.name);
				add_instruction(info, INSTRUCTION_SYMBOL);
				add_instruction(info, off);
			}
			++*stacklen;
			break;
		}
		case TYPE_CONS: {
			insert_literal(variant->data.cons.cdr, info, stacklen);
			insert_literal(variant->data.cons.car, info, stacklen);
			add_instruction(info, INSTRUCTION_CONS);
			--*stacklen;
			break;
		}
	};
}

struct builtin {
	char *name;
	int numargs;
	void (*compilation_func)(struct Variant *, struct compilation_info *, size_t *stacklen);
};

static void compile_quote(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	insert_literal(variant->data.cons.car, info, stacklen);
}

static void compile_car(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	compile_sexp(variant->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_MAKE_CONS);
	add_instruction(info, INSTRUCTION_CAR);
}

static void compile_cdr(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	compile_sexp(variant->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_MAKE_CONS);
	add_instruction(info, INSTRUCTION_CDR);
}

static void compile_not(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	compile_sexp(variant->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_NOT);
}

static void compile_and(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	compile_sexp(variant->data.cons.cdr->data.cons.car, info, stacklen);
	compile_sexp(variant->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_AND);
	--*stacklen;
}

static void compile_or(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	compile_sexp(variant->data.cons.cdr->data.cons.car, info, stacklen);
	compile_sexp(variant->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_OR);
	--*stacklen;
}

static void compile_eq(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	compile_sexp(variant->data.cons.cdr->data.cons.car, info, stacklen);
	compile_sexp(variant->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_EQ);
	--*stacklen;
}

static void compile_numeq(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	compile_sexp(variant->data.cons.cdr->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_MAKE_NUMBER);
	compile_sexp(variant->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_MAKE_NUMBER);
	add_instruction(info, INSTRUCTION_NUMCMP);
	--*stacklen;
}

static void compile_numlt(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	compile_sexp(variant->data.cons.cdr->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_MAKE_NUMBER);
	compile_sexp(variant->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_MAKE_NUMBER);
	add_instruction(info, INSTRUCTION_NUMCMP | (1 << 8));
	--*stacklen;
}

static void compile_numgt(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	compile_sexp(variant->data.cons.cdr->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_MAKE_NUMBER);
	compile_sexp(variant->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_MAKE_NUMBER);
	add_instruction(info, INSTRUCTION_NUMCMP | (2 << 8));
	--*stacklen;
}

static void compile_plus(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	compile_sexp(variant->data.cons.cdr->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_MAKE_NUMBER);
	compile_sexp(variant->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_MAKE_NUMBER);
	add_instruction(info, INSTRUCTION_PLUS);
	--*stacklen;
}

static void compile_minus(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	compile_sexp(variant->data.cons.cdr->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_MAKE_NUMBER);
	compile_sexp(variant->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_MAKE_NUMBER);
	add_instruction(info, INSTRUCTION_MINUS);
	--*stacklen;
}

static void compile_mul(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	compile_sexp(variant->data.cons.cdr->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_MAKE_NUMBER);
	compile_sexp(variant->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_MAKE_NUMBER);
	add_instruction(info, INSTRUCTION_MUL);
	--*stacklen;
}

static void compile_div(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	compile_sexp(variant->data.cons.cdr->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_MAKE_NUMBER);
	compile_sexp(variant->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_MAKE_NUMBER);
	add_instruction(info, INSTRUCTION_DIV);
	--*stacklen;
}

static void compile_gc(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	(void) variant;
	(void) stacklen;
	add_instruction(info, INSTRUCTION_GC);
}

static void compile_id(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	compile_sexp(variant->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_ID);
}

static void compile_format(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	size_t origstack = *stacklen;
	compile_application(variant, info, stacklen);
	size_t numargs = *stacklen - origstack;
	if (numargs == 0) {
		fprintf(stderr, "Too few arguments for builtin format\n");
		exit(EXIT_FAILURE);
	}
	add_instruction(info, INSTRUCTION_MAKE_STRING);
	add_instruction(info, INSTRUCTION_FORMAT | ((numargs - 1) << 8));
	*stacklen -= (numargs - 1);
}

static void compile_print(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	compile_sexp(variant->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_MAKE_STRING);
	add_instruction(info, INSTRUCTION_PRINT);
}

static void compile_read(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	(void) variant;
	add_instruction(info, INSTRUCTION_READ);
	++*stacklen;
}

static void compile_setq(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	if (variant->data.cons.car->type != TYPE_SYMBOL) {
		fprintf(stderr, "Invalid argument for setq, expected symbol\n");
		exit(EXIT_FAILURE);
	}
	compile_sexp(variant->data.cons.cdr->data.cons.car, info, stacklen);
	char *name = variant->data.cons.car->data.symbol.name;
	if (has_binding(info, name)) {
		add_instruction(info, INSTRUCTION_SET | ((*stacklen - 2 - get_binding(info, name)) << 8));
	} else {
		insert_literal(variant->data.cons.car, info, stacklen);
		add_instruction(info, INSTRUCTION_SETQ);
		--*stacklen;
	}
}

static void compile_setf(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	if (variant->data.cons.car->type == TYPE_SYMBOL) {
		return compile_setq(variant, info, stacklen);
	}
	compile_sexp(variant->data.cons.car, info, stacklen);
	compile_sexp(variant->data.cons.cdr->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_SETF);			// stackref is 0
	add_instruction(info, INSTRUCTION_POP | (1 << 8));	// cleanup duplicate variable
	--*stacklen;
}

static void compile_cons(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	compile_sexp(variant->data.cons.cdr->data.cons.car, info, stacklen);
	compile_sexp(variant->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_CONS);
	--*stacklen;
}

static void compile_defun(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	if (variant->type != TYPE_CONS) {
		fprintf(stderr, "Malformed function declaration\n");
		exit(EXIT_FAILURE);
	}

	struct Variant *name = variant->data.cons.car;
	if (name->type != TYPE_SYMBOL) {
		fprintf(stderr, "Invalid function name\n");
		exit(EXIT_FAILURE);
	}

	if (variant->data.cons.cdr->type != TYPE_CONS) {
		fprintf(stderr, "Malformed function declaration\n");
		exit(EXIT_FAILURE);
	}

	struct Variant *args = variant->data.cons.cdr->data.cons.car;
	uint16_t argcount = len(args);
	struct compilation_info ninfo = {
		.code = NULL,
		.code_size = 0,
		.bindings = NULL,
		.numbindings = 0,
		.constant_data = info->constant_data,
		.relocations = NULL,
	};
	size_t i = 0;
	while (args != &nil) {
		assert(args->type == TYPE_CONS);
		if (args->data.cons.car->type != TYPE_SYMBOL) {
			fprintf(stderr, "Invalid argument name\n");
			exit(EXIT_FAILURE);
		}
		add_binding(&ninfo, args->data.cons.car->data.symbol.name, argcount - 1 - i);
		++i;
		args = args->data.cons.cdr;
	}
	struct Variant *body = variant->data.cons.cdr->data.cons.cdr;

	size_t nstacklen = argcount;
	while (body != &nil) {
		assert(body->type == TYPE_CONS);
		compile_sexp(body->data.cons.car, &ninfo, &nstacklen);

		body = body->data.cons.cdr;
	}
	if (nstacklen == argcount) {
		insert_literal(&nil, &ninfo, &nstacklen);
	}

	add_instruction(&ninfo, INSTRUCTION_RET | ((nstacklen - 1) << 8));

	delete_bindings(&ninfo);

	info->constant_data = ninfo.constant_data;
	ninfo.constant_data = NULL;

	insert_literal(name, info, stacklen);
	add_instruction(info, INSTRUCTION_DEFUN | (argcount << 8));
	uint16_t offset = add_instruction(info, 0x1337);

	struct relocation *rel = malloc(sizeof(struct relocation));
	if (!rel) {
		perror("malloc");
		exit(EXIT_FAILURE);
	}

	rel->next = info->relocations;
	rel->data.compiled.nested = ninfo.relocations;
	rel->offset = offset;

	rel->iscompiled = true;
	rel->data.compiled.code = ninfo.code;
	rel->data.compiled.code_size = ninfo.code_size;

	info->relocations = rel;
}


static void compile_funcall(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	if (variant->type != TYPE_CONS) {
		fprintf(stderr, "Invalid funcall invokation");
		exit(EXIT_FAILURE);
	}

	size_t numargs = len(variant->data.cons.cdr);
	compile_application(variant->data.cons.cdr, info, stacklen);
	compile_sexp(variant->data.cons.car, info, stacklen);
	add_instruction(info, INSTRUCTION_MAKE_SYMBOL);
	add_instruction(info, INSTRUCTION_FUNCALL | (numargs << 8));
	*stacklen -= numargs;
}

static void compile_progn(struct Variant *body, struct compilation_info *info, size_t *stacklen) {
	if (body == &nil) {
		insert_literal(&nil, info, stacklen);
		return;
	}
	while (42) {
		assert(body->type == TYPE_CONS);
		size_t prevstack = *stacklen;
		compile_sexp(body->data.cons.car, info, stacklen);

		if (body->data.cons.cdr == &nil) break;

		while (*stacklen > prevstack) {
			add_instruction(info, (INSTRUCTION_POP | (1 << 8)));
			--*stacklen;
		}
		body = body->data.cons.cdr;
	}
}

static void compile_let(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	if (variant->type != TYPE_CONS) {
		fprintf(stderr, "Malformed let declaration\n");
		exit(EXIT_FAILURE);
	}

	struct Variant *bindings = variant->data.cons.car;
	if (bindings->type != TYPE_CONS) {
		fprintf(stderr, "Malformed let bindings\n");
		exit(EXIT_FAILURE);
	}

	struct Variant *curbind = bindings;
	while (curbind != &nil) {
		assert(curbind->type == TYPE_CONS);
		if (curbind->data.cons.car->type != TYPE_CONS || curbind->data.cons.car->data.cons.car->type != TYPE_SYMBOL) {
			fprintf(stderr, "Invalid let binding\n");
			exit(EXIT_FAILURE);
		}
		if (curbind->data.cons.car->data.cons.cdr == &nil) {
			insert_literal(&nil, info, stacklen);
			break;
		}
		if (curbind->data.cons.car->data.cons.cdr->data.cons.cdr != &nil) {
			fprintf(stderr, "Invalid let binding\n");
			exit(EXIT_FAILURE);
		}
		char *name = curbind->data.cons.car->data.cons.car->data.symbol.name;
		add_binding(info, name, *stacklen);
		compile_progn(curbind->data.cons.car->data.cons.cdr, info, stacklen);
		curbind = curbind->data.cons.cdr;
	}

	struct Variant *body = variant->data.cons.cdr;
	compile_progn(body, info, stacklen);

	curbind = bindings;
	while (curbind != &nil) {
		assert(curbind->type == TYPE_CONS);
		if (curbind->data.cons.car->type != TYPE_CONS || curbind->data.cons.car->data.cons.car->type != TYPE_SYMBOL) {
			fprintf(stderr, "Invalid let binding");
			exit(EXIT_FAILURE);
		}
		char *name = curbind->data.cons.car->data.cons.car->data.symbol.name;
		remove_binding(info, name);
		curbind = curbind->data.cons.cdr;
	}

	add_instruction(info, INSTRUCTION_PUSHDOWN | (len(bindings) << 8));
	*stacklen -= len(bindings);
}

static void compile_if(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	if (variant->type != TYPE_CONS) {
		fprintf(stderr, "Missing condition in if clause\n");
		exit(EXIT_FAILURE);
	}

	struct Variant *condition = variant->data.cons.car;

	if (variant->data.cons.cdr == &nil) {
		fprintf(stderr, "Missing true case in if clause\n");
		exit(EXIT_FAILURE);
	}

	compile_sexp(condition, info, stacklen);
	add_instruction(info, INSTRUCTION_CJMP | (1 << 8));
	uint16_t jmp1 = add_instruction(info, 0x1337);
	--*stacklen;

	struct Variant *tcase = variant->data.cons.cdr->data.cons.car;
	struct Variant *nilcase = variant->data.cons.cdr->data.cons.cdr;
	size_t orig_stacklen = *stacklen;
	compile_sexp(tcase, info, stacklen);

	add_instruction(info, INSTRUCTION_JMP);
	uint16_t jmp2 = add_instruction(info, 0x1337);

	uint16_t address = info->code_size;

	*stacklen = orig_stacklen;
	compile_progn(nilcase, info, stacklen);
	uint16_t end = info->code_size;

	struct relocation *tonil = malloc(sizeof(struct relocation));
	if (!tonil) {
		perror("malloc");
		exit(EXIT_FAILURE);
	}

	tonil->next = info->relocations;
	tonil->offset = jmp1;

	tonil->iscompiled = false;
	tonil->data.reference.jmptarget = address;

	info->relocations = tonil;

	struct relocation *toend = malloc(sizeof(struct relocation));
	if (!toend) {
		perror("malloc");
		exit(EXIT_FAILURE);
	}

	toend->next = info->relocations;
	toend->offset = jmp2;

	toend->iscompiled = false;
	toend->data.reference.jmptarget = end;

	info->relocations = toend;
}

static void compile_while(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	if (variant->type != TYPE_CONS) {
		fprintf(stderr, "Missing condition in while clause\n");
		exit(EXIT_FAILURE);
	}

	struct Variant *condition = variant->data.cons.car;

	struct Variant *body = variant->data.cons.cdr;

	add_instruction(info, INSTRUCTION_JMP);
	uint16_t jmp1 = add_instruction(info, 0x1337);

	uint16_t start = info->code_size;
	compile_progn(body, info, stacklen);
	add_instruction(info, INSTRUCTION_POP | (1 << 8));
	--*stacklen;
	uint16_t end = info->code_size;

	compile_sexp(condition, info, stacklen);
	add_instruction(info, INSTRUCTION_CJMP);
	uint16_t jmp2 = add_instruction(info, 0x1337);
	--*stacklen;
	insert_literal(&nil, info, stacklen);

	struct relocation *tocond = malloc(sizeof(struct relocation));
	if (!tocond) {
		perror("malloc");
		exit(EXIT_FAILURE);
	}

	tocond->next = info->relocations;
	tocond->offset = jmp1;

	tocond->iscompiled = false;
	tocond->data.reference.jmptarget = end;

	info->relocations = tocond;

	struct relocation *tobody = malloc(sizeof(struct relocation));
	if (!tobody) {
		perror("malloc");
		exit(EXIT_FAILURE);
	}

	tobody->next = info->relocations;
	tobody->offset = jmp2;

	tobody->iscompiled = false;
	tobody->data.reference.jmptarget = start;

	info->relocations = tobody;
}

static struct builtin builtins[] = {{.name = "quote", .numargs = 1, .compilation_func = compile_quote},
									{.name = "car", .numargs = 1, .compilation_func = compile_car},
									{.name = "cdr", .numargs = 1, .compilation_func = compile_cdr},
									{.name = "and", .numargs = 2, .compilation_func = compile_and},
									{.name = "not", .numargs = 1, .compilation_func = compile_not},
									{.name = "or", .numargs = 2, .compilation_func = compile_or},
									{.name = "eq", .numargs = 2, .compilation_func = compile_eq},
									{.name = "=", .numargs = 2, .compilation_func = compile_numeq},
									{.name = "<", .numargs = 2, .compilation_func = compile_numlt},
									{.name = ">", .numargs = 2, .compilation_func = compile_numgt},
									{.name = "+", .numargs = 2, .compilation_func = compile_plus},
									{.name = "-", .numargs = 2, .compilation_func = compile_minus},
									{.name = "*", .numargs = 2, .compilation_func = compile_mul},
									{.name = "/", .numargs = 2, .compilation_func = compile_div},
									{.name = "id", .numargs = 1, .compilation_func = compile_id},
									{.name = "gc", .numargs = 0, .compilation_func = compile_gc},
									{.name = "format", .numargs = -1, .compilation_func = compile_format},
									{.name = "print", .numargs = 1, .compilation_func = compile_print},
									{.name = "read", .numargs = 0, .compilation_func = compile_read},
									{.name = "setq", .numargs = 2, .compilation_func = compile_setq},
									{.name = "setf", .numargs = 2, .compilation_func = compile_setf},
									{.name = "cons", .numargs = 2, .compilation_func = compile_cons},
									{.name = "defun", .numargs = -1, .compilation_func = compile_defun},
									{.name = "funcall", .numargs = -1, .compilation_func = compile_funcall},
									{.name = "let", .numargs = -1, .compilation_func = compile_let},
									{.name = "progn", .numargs = -1, .compilation_func = compile_progn},
									{.name = "if", .numargs = -1, .compilation_func = compile_if},
									{.name = "while", .numargs = -1, .compilation_func = compile_while}};

static void compile_sexp(struct Variant *variant, struct compilation_info *info, size_t *stacklen) {
	switch (variant->type) {
		case TYPE_NUMBER: {
			uint16_t off = add_number(info, variant->data.number);
			add_instruction(info, INSTRUCTION_NUMBER);
			add_instruction(info, off);
			++*stacklen;
			break;
		}
		case TYPE_STRING: {
			uint16_t off = add_string(info, variant->data.string.value);
			add_instruction(info, INSTRUCTION_STRING);
			add_instruction(info, off);
			++*stacklen;
			break;
		}
		case TYPE_SYMBOL: {
			if (has_binding(info, variant->data.symbol.name)) {
				add_instruction(info,
								INSTRUCTION_DUP | ((*stacklen - 1 - get_binding(info, variant->data.symbol.name)) << 8));
			} else if (strcmp(variant->data.symbol.name, "nil") == 0) {
				add_instruction(info, INSTRUCTION_NIL);
			} else if (strcmp(variant->data.symbol.name, "t") == 0) {
				add_instruction(info, INSTRUCTION_T);
			} else {
				uint16_t off = add_string(info, variant->data.symbol.name);
				add_instruction(info, INSTRUCTION_SYMBOL);
				add_instruction(info, off);
				add_instruction(info, INSTRUCTION_VAR);
			}
			++*stacklen;
			break;
		}
		case TYPE_CONS: {
			if (variant->data.cons.car->type != TYPE_SYMBOL) {
				fprintf(stderr, "Malformed function application\n");
				exit(EXIT_FAILURE);
			}

			// builtins
			for (size_t i = 0; i < sizeof(builtins) / sizeof(*builtins); ++i) {
				if (strcmp(variant->data.cons.car->data.symbol.name, builtins[i].name) == 0) {
					if (builtins[i].numargs >= 0 && len(variant->data.cons.cdr) != builtins[i].numargs) {
						fprintf(stderr, "Invalid number of arguments for builtin %s\n", builtins[i].name);
						exit(EXIT_FAILURE);
					}
					builtins[i].compilation_func(variant->data.cons.cdr, info, stacklen);
					return;
				}
			}

			size_t origstack = *stacklen;
			compile_application(variant->data.cons.cdr, info, stacklen);
			size_t numargs = *stacklen - origstack;

			insert_literal(variant->data.cons.car, info, stacklen);
			add_instruction(info, INSTRUCTION_FUNCALL | (numargs << 8));

			*stacklen -= numargs;
			break;
		}
	}
}

static void process_relocations(struct compilation_info *info, struct relocation *relocation, uint16_t offset) {
	while (relocation) {
		if (relocation->iscompiled) {
			uint16_t origsize = info->code_size;
			info->code_size += relocation->data.compiled.code_size;
			info->code = reallocarray(info->code, sizeof(uint16_t), info->code_size);
			if (!info->code) {
				perror("reallocarray");
				exit(EXIT_FAILURE);
			}

			memcpy(info->code + origsize, relocation->data.compiled.code,
				   relocation->data.compiled.code_size * sizeof(uint16_t));
			info->code[offset + relocation->offset] = origsize;
			struct relocation *nested = relocation->data.compiled.nested;

			free(relocation->data.compiled.code);
			process_relocations(info, nested, origsize);
		} else {
			info->code[offset + relocation->offset] = offset + relocation->data.reference.jmptarget;
		}

		struct relocation *next = relocation->next;

		free(relocation);

		relocation = next;
	}
}

static void delete_trie(struct Trie *t) {
	if (!t) return;

	for (size_t i = 0; i < 256; ++i) {
		delete_trie(t->next[i]);
	}
	free(t);
}

struct compilation_info compile(FILE *f) {
	struct constant_data *data = malloc(sizeof(struct constant_data));
	data->constants = NULL;
	data->constants_size = 0;
	data->root = NULL;

	struct compilation_info info = {
		.code = NULL, .constant_data = data, .bindings = NULL, .numbindings = 0, .relocations = NULL, .code_size = 0};
	struct Variant *variant;
	size_t stacklen = 0;
	while ((variant = parse_sexp(NULL, f))) {
		compile_sexp(variant, &info, &stacklen);
		delete_variant(variant);
	}
	if (parsing_error)
		exit(EXIT_FAILURE);
	if (!feof(f)) {
		fprintf(stderr, "Too many closing parens\n");
		exit(EXIT_FAILURE);
	}
	if (stacklen == 0) {
		insert_literal(&nil, &info, &stacklen);
	}
	add_instruction(&info, INSTRUCTION_RET | ((stacklen - 1) << 8));

	process_relocations(&info, info.relocations, 0);
	info.relocations = NULL;
	delete_trie(data->root);
	delete_bindings(&info);

	return info;
}
