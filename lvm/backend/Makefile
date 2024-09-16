.PHONY: all

CC=gcc
CCFLAGS=-O3 -g -Wall -Wextra -Werror -no-pie
# -fsanitize=address

all: ../lisp ../lispc

../lisp: main.o vm.o instruction.o map.o parser.o
	$(CC) $(CCFLAGS) -o ../lisp $^
../lispc: cmain.o compiler.o parser.o
	$(CC) $(CCFLAGS) -o ../lispc $^

cmain.o: cmain.c compiler.h error.h vm.h
compiler.o: compiler.c compiler.h error.h instruction.h vm.h parser.h
instruction.o: instruction.c instruction.h vm.h
main.o: main.c error.h instruction.h vm.h
map.o: map.c map.h error.h
parser.o: parser.c parser.h vm.h
vm.o: vm.c vm.h error.h instruction.h map.h parser.h

%.o: %.c
	$(CC) $(CCFLAGS) -c -o $@ $<

clean:
	rm -f main.o vm.o instruction.o map.o compiler.o cmain.o parser.o
