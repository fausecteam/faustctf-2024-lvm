#include "instruction.h"

#include "vm.h"

struct instruction parse_instruction(struct vm *vm) {
	struct instruction ret = {};

	uint16_t opcode = vm_next_opcode(vm);
	ret.type = opcode & 0xff;

	switch (ret.type) {
		case INSTRUCTION_DEFUN:
			ret.numargs = opcode >> 8;
			/* fallthrough */
		case INSTRUCTION_JMP:
		case INSTRUCTION_SYMBOL:
		case INSTRUCTION_STRING:
		case INSTRUCTION_NUMBER:
			ret.address = vm_next_opcode(vm);
			break;
		case INSTRUCTION_CJMP:
			ret.cond = opcode >> 8;
			ret.address = vm_next_opcode(vm);
			break;
		case INSTRUCTION_NUMCMP:
			ret.cond = opcode >> 8;
			if (ret.cond > 2) {
				ret.type = UNDEFINED_INSTRUCTION;
			}
			break;
		case INSTRUCTION_NIL:
		case INSTRUCTION_T:
		case INSTRUCTION_PLUS:
		case INSTRUCTION_MINUS:
		case INSTRUCTION_MUL:
		case INSTRUCTION_DIV:
		case INSTRUCTION_CAR:
		case INSTRUCTION_CDR:
		case INSTRUCTION_AND:
		case INSTRUCTION_NOT:
		case INSTRUCTION_OR:
		case INSTRUCTION_EQ:
		case INSTRUCTION_VAR:
		case INSTRUCTION_CONS:
		case INSTRUCTION_SYMBOLP:
		case INSTRUCTION_STRINGP:
		case INSTRUCTION_CONSP:
		case INSTRUCTION_NUMBERP:
		case INSTRUCTION_MAKE_SYMBOL:
		case INSTRUCTION_MAKE_STRING:
		case INSTRUCTION_MAKE_CONS:
		case INSTRUCTION_MAKE_NUMBER:
		case INSTRUCTION_SETQ:
		case INSTRUCTION_GC:
		case INSTRUCTION_ID:
		case INSTRUCTION_PRINT:
		case INSTRUCTION_READ:
			break;
		case INSTRUCTION_DUP:
		case INSTRUCTION_POP:
		case INSTRUCTION_PUSHDOWN:
		case INSTRUCTION_SET:
		case INSTRUCTION_SETF:
		case INSTRUCTION_FORMAT:
		case INSTRUCTION_RET:
			ret.stackref = opcode >> 8;
			break;
		case INSTRUCTION_FUNCALL:
			ret.numargs = opcode >> 8;
			break;
		default:
			ret.type = UNDEFINED_INSTRUCTION;
	}
	return ret;
}

void print_instruction(struct vm *vm, struct instruction *ins) {
	switch (ins->type) {
		case INSTRUCTION_FUNCALL:
			printf("FUNCALL%zu", ins->numargs);
			break;
		case INSTRUCTION_DEFUN:
			printf("DEFUN%-3zu    %04x", ins->numargs, ins->address);
			break;
		case INSTRUCTION_JMP:
			printf("JMP         %04x", ins->address);
			break;
		case INSTRUCTION_CJMP:
			if (ins->cond)
				printf("JMP-IF-NIL  %04x", ins->address);
			else
				printf("JMP-IF-T    %04x", ins->address);
			break;
		case INSTRUCTION_SYMBOL:
			printf("CONSTANT   '%s", vm_get_string(vm, ins->address));
			break;
		case INSTRUCTION_STRING:
			printf("CONSTANT   \"%s\"", vm_get_string(vm, ins->address));
			break;
		case INSTRUCTION_NUMBER:
			printf("CONSTANT    %lf", vm_get_number(vm, ins->address));
			break;
		case INSTRUCTION_NIL:
			printf("NIL");
			break;
		case INSTRUCTION_T:
			printf("T");
			break;
		case INSTRUCTION_PLUS:
			printf("PLUS");
			break;
		case INSTRUCTION_MINUS:
			printf("MINUS");
			break;
		case INSTRUCTION_MUL:
			printf("MUL");
			break;
		case INSTRUCTION_DIV:
			printf("DIV");
			break;
		case INSTRUCTION_CAR:
			printf("CAR");
			break;
		case INSTRUCTION_CDR:
			printf("CDR");
			break;
		case INSTRUCTION_AND:
			printf("AND");
			break;
		case INSTRUCTION_NOT:
			printf("NOT");
			break;
		case INSTRUCTION_OR:
			printf("OR");
			break;
		case INSTRUCTION_EQ:
			printf("EQ");
			break;
		case INSTRUCTION_NUMCMP:
			switch (ins->cond) {
				case 0:
					printf("NUMEQ");
					break;
				case 1:
					printf("NUMLT");
					break;
				case 2:
					printf("NUMGT");
					break;
			}
			break;
		case INSTRUCTION_VAR:
			printf("VAR");
			break;
		case INSTRUCTION_CONS:
			printf("CONS");
			break;
		case INSTRUCTION_SYMBOLP:
			printf("SYMBOLP");
			break;
		case INSTRUCTION_STRINGP:
			printf("STRINGP");
			break;
		case INSTRUCTION_CONSP:
			printf("CONSP");
			break;
		case INSTRUCTION_NUMBERP:
			printf("NUMBERP");
			break;
		case INSTRUCTION_MAKE_SYMBOL:
			printf("MAKE-SYMBOL");
			break;
		case INSTRUCTION_MAKE_STRING:
			printf("MAKE-STRING");
			break;
		case INSTRUCTION_MAKE_CONS:
			printf("MAKE-CONS");
			break;
		case INSTRUCTION_MAKE_NUMBER:
			printf("MAKE-NUMBER");
			break;
		case INSTRUCTION_POP:
			printf("POP         %zu", ins->stackref);
			break;
		case INSTRUCTION_PUSHDOWN:
			printf("PUSHDOWN    %zu", ins->stackref);
			break;
		case INSTRUCTION_DUP:
			printf("DUP         %zu", ins->stackref);
			break;
		case INSTRUCTION_SET:
			printf("SET         %zu", ins->stackref);
			break;
		case INSTRUCTION_SETF:
			printf("SETF        %zu", ins->stackref);
			break;
		case INSTRUCTION_SETQ:
			printf("SETQ");
			break;
		case INSTRUCTION_GC:
			printf("GC");
			break;
		case INSTRUCTION_ID:
			printf("ID");
			break;
		case INSTRUCTION_PRINT:
			printf("PRINT");
			break;
		case INSTRUCTION_READ:
			printf("READ");
			break;
		case INSTRUCTION_FORMAT:
			printf("FORMAT      %zu", ins->stackref);
			break;
		case INSTRUCTION_RET:
			printf("RET         %zu", ins->stackref);
			break;
		default:
			printf("???");
	}
}
