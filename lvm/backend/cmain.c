#include <libgen.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "compiler.h"
#include "error.h"
#include "vm.h"

int main(int argc, char **argv) {
	if (argc < 2) {
		fprintf(stderr, "Usage: lispc <filename...>\n");
		exit(EXIT_FAILURE);
	}

	for (int i = 1; i < argc; ++i) {
		FILE *f;
		if (strcmp(argv[i], "-") == 0) {
			f = fdopen(dup(STDIN_FILENO), "r");
			argv[i] = strdup("stdin");
		} else {
			f = fopen(argv[i], "r");
			argv[i] = strdup(argv[i]);
		}

		if (!f) {
			perror("fopen");
			exit(EXIT_FAILURE);
		}

		struct compilation_info info = compile(f);
		fclose(f);

		char *lastsep = strrchr(argv[i], '/');
		if (!lastsep) {
			lastsep = argv[i];
		}

		char *dot = strchr(lastsep, '.');
		if (dot) {
			*dot = '\0';
		}

		// allocate output filename
		char *output = malloc(strlen(argv[i]) + 7);
		strcpy(output, argv[i]);
		strcat(output, ".lispc");

		f = fopen(output, "w");
		free(output);
		free(argv[i]);

		struct bytecode_header header;
		header.version = 1;
		memcpy(header.magic, "LSP", 3);
		header.codelength = info.code_size;
		header.constantlength = info.constant_data->constants_size;
		header.fileoffset = sizeof(header);
		if (fwrite(&header, sizeof(header), 1, f) != 1) {
			perror("fwrite");
			exit(EXIT_FAILURE);
		}
		if (fwrite(info.code, sizeof(uint16_t), info.code_size, f) != info.code_size) {
			perror("fwrite");
			exit(EXIT_FAILURE);
		}
		if (fwrite(info.constant_data->constants, 1, info.constant_data->constants_size, f) !=
			info.constant_data->constants_size) {
			perror("fwrite");
			exit(EXIT_FAILURE);
		}

		fclose(f);

		free(info.code);
		free(info.constant_data->constants);
		free(info.constant_data);
	}
}
