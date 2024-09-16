#define _GNU_SOURCE
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <unistd.h>
#include <grp.h>

#include "error.h"
#include "instruction.h"
#include "vm.h"

void sandbox() {
	uint32_t uid;
	FILE *rand = fopen("/dev/urandom", "r");
	assert(fread(&uid, sizeof(uid), 1, rand) == 1);
	fclose(rand);

	assert(setgroups(0, NULL) == 0);
	assert(setresgid(uid, uid, uid) == 0);
	assert(setresuid(uid, uid, uid) == 0);

	struct rlimit proc_lim = {
		.rlim_cur = 50,
		.rlim_max = 50,
	};
	setrlimit(RLIMIT_NPROC, &proc_lim);
	struct rlimit cpu_lim = {
		.rlim_cur = 10,
		.rlim_max = 10,
	};
	setrlimit(RLIMIT_CPU, &cpu_lim);
}

int main(int argc, char **argv) {
	setvbuf(stdout, NULL, _IONBF, 0);
	setvbuf(stderr, NULL, _IONBF, 0);
	sandbox();

	if (argc >= 2 && argv[1][0] != '-') {
		FILE *f = fopen(argv[1], "r");

		if (!f) {
			perror("fopen");
			exit(EXIT_FAILURE);
		}

		struct vm *vm = create_vm(f);
		fclose(f);

		struct Variant *res = vm_run(vm);

		printf("Application returned: ");
		print_variant(res);

		putchar(10);

		delete_vm(vm);
	} else if (argc == 3 && strcmp(argv[1], "--disasm") == 0) {
		FILE *f = fopen(argv[2], "r");

		if (!f) {
			perror("fopen");
			exit(EXIT_FAILURE);
		}

		struct vm *vm = create_vm(f);
		fclose(f);

		struct instruction ins;
		do {
			uint16_t ip = vm_get_ip(vm);
			ins = parse_instruction(vm);
			printf("%04x: ", ip);
			print_instruction(vm, &ins);
			putchar('\n');
		} while (ins.type != UNDEFINED_INSTRUCTION);
		delete_vm(vm);
	} else {
		fprintf(stderr, "Usage: lvm [--disasm] <filename>\n");
		exit(EXIT_FAILURE);
	}
}
