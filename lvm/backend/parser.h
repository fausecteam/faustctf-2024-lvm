#ifndef PARSER_H
#define PARSER_H

#include <stdio.h>
#include <stdbool.h>

#include "vm.h"

extern bool parsing_error;
struct Variant *parse_sexp(struct vm *vm, FILE *f);

#endif
