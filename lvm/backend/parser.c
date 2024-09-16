#include "parser.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

bool parsing_error;

static char *parse_string(FILE *f) {
	int c = getc(f);

	char *res = NULL;
	size_t size = 0;
	while (c != '"') {
		if (c == EOF) {
		fail:
			fprintf(stderr, "Unexpected EOF in string literal\n");
			parsing_error = true;
			free(res);
			return NULL;
		}

		if (c == '\\') {
			c = getc(f);
			if (c == EOF) {
				goto fail;
			}
			switch(c) {
			case 'n':
				c = '\n';
				break;
			case 't':
				c = '\t';
				break;
			case 'r':
				c = '\r';
				break;
			case '\\':
				c = '\\';
				break;
			default:
				fprintf(stderr, "Unexpected escape sequence in string literal\n");
				parsing_error = true;
				free(res);
				return NULL;
			}
		}

		++size;
		res = realloc(res, size);
		if (!res) {
			perror("realloc");
			exit(EXIT_FAILURE);
		}
		res[size - 1] = c;
		c = getc(f);
	}
	res = realloc(res, size + 1);
	if (!res) {
		perror("realloc");
		exit(EXIT_FAILURE);
	}
	res[size] = '\0';

	return res;
}

static bool symbolp(int c) { return isprint(c) && !isspace(c) && c != '"' && c != '(' && c != ')'; }

static char *parse_symbol(FILE *f) {
	int c = getc(f);
	if (!symbolp(c)) {
		fprintf(stderr, "Invalid symbol\n");
		parsing_error = true;
		return NULL;
	}

	char *res = NULL;
	size_t size = 0;
	while (symbolp(c)) {
		++size;
		res = realloc(res, size);
		if (!res) {
			perror("realloc");
			exit(EXIT_FAILURE);
		}
		res[size - 1] = c;
		c = getc(f);
	}
	res = realloc(res, size + 1);
	if (!res) {
		perror("realloc");
		exit(EXIT_FAILURE);
	}
	res[size] = '\0';

	if (c != EOF) ungetc(c, f);

	return res;
}

static struct Variant *parse_list(struct vm *vm, FILE *f) {
	int c = fgetc(f);
	if (c == EOF) {
		fprintf(stderr, "Unexpected EOF in list\n");
		parsing_error = true;
		return NULL;
	}

	if (c == ')') return &nil;

	ungetc(c, f);
	struct Variant *car = parse_sexp(vm, f);
	if (!car) {
		// parse_sexp doesn't set error on EOF
		return parse_list(vm, f);
	}
	struct Variant *cdr = parse_list(vm, f);
	if (!cdr)
		// car will be cleaned up by gc
		return NULL;

	struct Variant *res = variant_new(vm, TYPE_CONS);
	res->data.cons.car = car;
	res->data.cons.cdr = cdr;
	return res;
}

struct Variant *parse_sexp(struct vm *vm, FILE *f) {
	parsing_error = false;
	while (42) {
		int c = fgetc(f);
		while (isspace(c)) {
			c = fgetc(f);
		}
		if (c == EOF) return NULL;

		switch (c) {
			case '(': {
				return parse_list(vm, f);
			}
			case ')':
				ungetc(c, f);
				return NULL;
			case '"': {
				char *string = parse_string(f);
				if (!string) return NULL;

				struct Variant *res = variant_new(vm, TYPE_STRING);
				res->data.string.constant = false;
				res->data.string.value = string;
				return res;
			}
			case '\'': {
				struct Variant *arg = parse_sexp(vm, f);
				if (!arg) {
					fprintf(stderr, "Unexpected EOF in quote");
					parsing_error = true;
					return NULL;
				}

				struct Variant *res = variant_new(vm, TYPE_CONS);
				res->data.cons.car = variant_new(vm, TYPE_SYMBOL);
				res->data.cons.car->data.symbol.constant = true;
				res->data.cons.car->data.symbol.name = "quote";
				res->data.cons.cdr = variant_new(vm, TYPE_CONS);
				res->data.cons.cdr->data.cons.car = arg;
				res->data.cons.cdr->data.cons.cdr = &nil;
				return res;
			}
			case '-': {
				int nchar = fgetc(f);
				ungetc(nchar, f);
				if (!(nchar >= '0' && nchar <= '9')) goto symbol;
			}
				/* fallthrough */
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
			case '.': {
				ungetc(c, f);
				double number;
				if (fscanf(f, "%lf", &number) != 1) {
					fprintf(stderr, "Invalid number literal\n");
					parsing_error = true;
					return NULL;
				}
				struct Variant *res = variant_new(vm, TYPE_NUMBER);
				res->data.number = number;
				return res;
			}
			case ';':
				c = fgetc(f);
				while (c != EOF && c != '\n') c = fgetc(f);
				break;
			default: {
			symbol:
				ungetc(c, f);
				char *symbol = parse_symbol(f);
				if (!symbol) return NULL;
				struct Variant *res = variant_new(vm, TYPE_SYMBOL);
				res->data.symbol.constant = false;
				res->data.symbol.name = symbol;
				return res;
			}
		}
	}
}
