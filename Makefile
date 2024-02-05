CC = clang
AR = ar
CFLAGS = -Wall -Wextra -Werror
LDFLAGS = -L./runtime -lruntime
CDEBUGFLAGS = -Wall -Wextra -Werror -g

.PHONY: clean debug all

all: runVMtest

runtime.o: ./runtime/runtime.c ./runtime/runtime.c
	clang $(CDEBUGFLAGS) -c ./runtime/runtime.c -o runtime.o

libruntime.a: runtime.o
	$(AR) rcs libruntime.a runtime.o

runVMtest: libruntime.a runtime.o
	$(CC) $(CDEBUGFLAGS) ./test/runningVMtest.c -o runVMtest -L./ -lruntime -Iruntime


clean:
	@rm -f *.o *.a
	@rm runVMtest
