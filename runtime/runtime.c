#include "runtime.h"

Trap runAs(Mode mode)
{
	if(mode == CLI)
	{
		runtimeCLIListen();
	}
	else
	{
		return TRAP_COULD_NOT_START;
	}
	return TRAP_OK;
}

void sanitizeCStr(char* cstr)
{
		// Remove newline character from the input
		// and replace with null terminator
		size_t length = strlen(cstr);
		if (length > 0 && cstr[length - 1] == '\n') 
				cstr[length - 1] = '\0';
}

void runtimeCLIListen()
{
	Runtime runtime;
	runtime.mode = CLI;
	runtime.parserType = CMD;
	runtime.indent = 0;
	runtime.stackLen = 0;
	runtime.instructionIndex = 0;
	while(1)
	{
		while(1)
		{
			char* userInput = (char*) malloc(sizeof(char) * 24);
			fgets(userInput,24, stdin);
			sanitizeCStr(userInput);
			if(userInput[0] == ':')
			{
				Trap result = listenREPLCmd(&runtime, userInput);
				if(result == TRAP_OK)
				{
					continue;
				}
				else if(result == TRAP_EXEC)
				{
					break;
				}
				else
				{
					printf("Command error\n");
					continue;
				}
			}
			else
			{
				Trap result = listenREPLBytecode(&runtime,userInput);
				if(result == TRAP_OK)
				{
					continue;
				}
			}
		}
		executeStack(&runtime);
	}
}

Trap pushStack(Runtime* runtime,void* value)
{
	if(runtime->stackLen < STACK_CAPACITY)
	{
		if(runtime->stackLen != 0)
		{
			for(size_t i=(runtime->stackLen);i>0;i--)
			{
				runtime->word[i] = runtime->word[i-1];
			}
			runtime->word[0] = value;
		}
		else
		{
			runtime->word[runtime->stackLen] = value;
		}
		runtime->stackLen++;
		return TRAP_OK;
	}
	else
	{
		return TRAP_STACK_OVERFLOW;
	}
}

Trap popStack(Runtime* runtime)
{
	if(runtime->stackLen > 0)
	{
		runtime->word[0] = (void*) 0;
		return TRAP_OK;
	}
	else
	{
		return TRAP_STACK_UNDERFLOW;
	}
}

Trap executeStack(Runtime* runtime)
{
	for(;runtime->instructionIndex<runtime->stackLen;runtime->instructionIndex++)
	{
		// printf("Running %ld\n",runtime->instructionIndex);
		if(strcmp((char*) runtime->word[runtime->instructionIndex], "natural") == 0)
		{
			runtime->instructionIndex = runtime->instructionIndex + 1;
			int64_t* nat = (int64_t*) malloc(sizeof(int64_t));
			BytecodeError status = cstrToNatural((char*) runtime->word[runtime->instructionIndex],&nat);
			if(status == CSTR_NATURAL_ERROR)
			{
				printf("Could not parse to natural\n");
			}
			else if(status == CSTR_UNDEFINED)
			{
				printf("Undefined literal\n");
			}
			else
			{
				runtime->word[runtime->instructionIndex] = (void*) nat;
			}
		}
		else if(strcmp((char*) runtime->word[runtime->instructionIndex], "matrix") == 0)
		{
		}
		else if(strcmp((char*) runtime->word[runtime->instructionIndex], "vector") == 0)
		{
		}
		else if(strcmp((char*) runtime->word[runtime->instructionIndex], "character") == 0)
		{
		}
		else if(strcmp((char*) runtime->word[runtime->instructionIndex], "add") == 0)
		{
			operate_add(runtime);
		}
		else if(strcmp((char*) runtime->word[runtime->instructionIndex], "factorial") == 0)
		{
		}
		else if(strcmp((char*) runtime->word[runtime->instructionIndex], "mul") == 0)
		{

		}
		else if(strcmp((char*) runtime->word[runtime->instructionIndex], "div") == 0)
		{

		}
		else if(strcmp((char*) runtime->word[runtime->instructionIndex], "function") == 0)
		{

		}
		else if(strcmp((char*) runtime->word[runtime->instructionIndex], "call") == 0)
		{

		}
		else if(strcmp((char*) runtime->word[runtime->instructionIndex], "return") == 0)
		{

		}
		else if(strcmp((char*) runtime->word[runtime->instructionIndex], "unknown") == 0)
		{

		}
		else
		{
			printf("()\n");
			break;
		}
	} 
	return TRAP_OK;
}

BytecodeError cstrToNatural(char* literal, int64_t** natural)
{
		if(literal != NULL)
		{
			//this should not be malloced strtol does it for you
			char* rest; 
			int64_t nat_temp = strtol(literal,&rest,10);
			**natural = nat_temp;
			if(strlen(rest) == 0)
			{
				return BYTECODE_OK;
			}
			else
			{
				printf("%s<<<",literal);
				return CSTR_NATURAL_ERROR;
			}
		}
		else
		{
				return CSTR_UNDEFINED;
		}
}

void printNatural(void* value)
{
	printf("%ld\n", *(int64_t*) value);
}

void REPLprint(Runtime* runtime,char* cstr)
{
	printf("%*s\n",(int) runtime->indent,cstr);
}

void dumpStack(Runtime* runtime)
{
	if(runtime->stackLen > 0)
	{
		printf("**Dump\n");
		for(size_t i=0;i<runtime->stackLen;i++)
		{
			printf("%*zu %s\n",3,i,(char*) runtime->word[i]);
		}
		printf("**\n");
	}
	else
	{
		printf("**empty[]**\n");
	}
}

void loadFile(char* filePath,Runtime* runtime)
{
	FILE* hFile;
	hFile = fopen(filePath,"rb");
	if(strcmp(&filePath[strlen(filePath)-4],".erb") == 0)
	{
		//Load Bytecode
		if(hFile != NULL)
		{
			fseek(hFile,0,SEEK_END);
			long len = ftell(hFile);
			fseek(hFile,0,SEEK_SET);
			char* source = (char*) malloc(sizeof(char) * len);
			char** res = (char**) malloc(sizeof(char*) * len);
			fread(source,1,len,hFile);	
			if(source != NULL)
			{
				fclose(hFile);
				size_t count=0;
				size_t start=0;
				for(int i=0; i<=len;i++)
				{
					if(source[i] == ' ' || source[i] == '\n' || source[i] == '\0')
					{
							int l = i - start;
							if(l>0)
							{
								res[count] = (char*) malloc((len + 1) * sizeof(char));
								strncpy(res[count], &source[start], len);
								res[count][l] = '\0';
								count++;
							}
							start = i + 1;
					}
				}
				for(size_t i=0;i<count;i++)
				{
					pushStack(runtime,res[i]);
				}		
				REPLprint(runtime,"Loaded");
			}
		}
		else
		{
			printf("nope\n");
		}
	}
	else
	{
		//Load Eureka
		printf("Not implemented yet");
	}
}

Trap listenREPLCmd(Runtime* runtime,char* userInput)
{
	runtime->indent = 0;
	if(userInput[0] == ':')
	{	
		char command[24];
		size_t i = 0;
		for(;userInput[i+1] != ' ' && i<strlen(userInput);i++)
		{
			command[i] = userInput[i+1];
		}

		if(strcmp("load",command) == 0)
		{
			//index i+2 is where the space is
			loadFile(userInput+i+2,runtime);
		}
		else if(strcmp("dump",command) == 0)
		{
			dumpStack(runtime);
		}
		else if(strcmp("echo",command) == 0)
		{
			REPLprint(runtime,userInput+i+2);
		}
		else if(strcmp("btm",command) == 0)
		{
			printf("BYTECODE:\n");
			runtime->parserType = BT;
		}
		else if(strcmp("exec",command) == 0)
		{
			printf("Executing stack...\n");
			return TRAP_EXEC;
		}
		else
		{
			return TRAP_COMMAND_NOT_FOUND;
		}
		return TRAP_OK;
	}
	else
	{
		return TRAP_NOT_COMMAND;
	}
}

Trap listenREPLBytecode(Runtime* runtime,char* userInput)
{
	if(strlen(userInput) > 0)
	{
		runtime->indent = 4;
		pushStack(runtime,userInput);
	}
	return TRAP_OK;
}

void operate_add(Runtime* runtime)
{	
	size_t i,j;
	for(i = runtime->instructionIndex;i>=0;i--)
	{
		if(strcmp((char*) runtime->word[i],"natural") == 0)
		{
			for(j=i-1;j>=0;j--)
			{			
				if(strcmp((char*) runtime->word[j],"natural") == 0)
				{
					int64_t res =  *(int64_t*) runtime->word[i+1] + *(int64_t*) runtime->word[j+1];

					runtime->word[runtime->instructionIndex] = (void*) &res;						

				break;
				}
			}
			break;
		}
		else if(strcmp((char*) runtime->word[i],"matrix") == 0)
		{
			
		}
		else if(strcmp((char*) runtime->word[i],"character") == 0)
		{

		}
		else if(strcmp((char*) runtime->word[i],"vector") == 0)
		{

		}
		else if(strcmp((char*) runtime->word[i],"function") == 0)
		{

		}
	}
}
