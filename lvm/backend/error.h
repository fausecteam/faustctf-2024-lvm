#ifndef ERROR_H
#define ERROR_H

#include <stdio.h>

#define _STRINGIFY_INNER(x) #x
#define STRINGIFY(x) _STRINGIFY_INNER(x)
#define perror(msg) perror(__FILE__ ":" STRINGIFY(__LINE__) " " msg)

#endif
