#ifndef RUNTIME_H
#define RUNTIME_H

#define STACK_CAPACITY 1025

#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

typedef enum
{
	CLI,
	DAEMON,
}Mode;

typedef enum
{
	WOW,
}Word;

typedef enum
{
	BT,
	CODE,
	CMD,
}ParserType;

typedef union 
{
	uint64_t i;
	char c[10];
}Parser;

typedef enum
{
	TRAP_OK,
	TRAP_STACK_UNDERFLOW,
	TRAP_STACK_OVERFLOW,
	TRAP_COMPILE_ERROR,
	TRAP_COULD_NOT_START,
	TRAP_COMMAND_NOT_FOUND,
	TRAP_EXEC,
	TRAP_NOT_COMMAND,
}Trap;

typedef enum
{
	ADD,
}Operation;

typedef struct
{
	Mode mode;	
	void* word[STACK_CAPACITY];
	ParserType parserType;
	size_t stackLen;
	size_t indent;
	size_t instructionIndex;
	Parser* parser;
}Runtime;

Trap runAs(Mode);
Trap pushStack(Runtime*,void*);
Trap popStack(Runtime*);
void resizeStack(Runtime*);
void runtimeCLIListen();
void sanitizeCStr(char*);

Trap executeStack(Runtime*);
void dumpStack(Runtime*);
void runtimeOut(Runtime*,char*);

// DataType functions
typedef enum 
{
 BYTECODE_OK,	
 CSTR_NATURAL_ERROR,
 CSTR_UNDEFINED,
}BytecodeError;

typedef enum
{
	Natural,
}DataType;

void printNatural(void*);
void printStackValue(void*);
void REPLprint(Runtime* runtime,char* cstr);
void loadFile(char*,Runtime* runtime);

BytecodeError cstrToNatural(char*,int64_t**);

Trap listenREPLCmd(Runtime*,char*);
Trap listenREPLBytecode(Runtime*,char*);

void operate_add(Runtime* runtime);
#endif //RUNTIME_H
