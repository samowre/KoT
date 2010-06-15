.SUFFIXES: .states
.c.states:
	./states $*.c
.c.o:
	$(CC) $(CFLAGS) -c $*.c

CC=gcc
CFLAGS=-Wall -W -Werror -O3 -fomit-frame-pointer -march=i686
AR=ar
RANLIB=ranlib

OBJ=booleforce.o bfmem.o bftime.o bfio.o bfnum.o

all: booleforce tracecheck

clean:
	rm -f config.h Makefile
	rm -f *.states
	rm -f *.o *.a
	rm -f booleforce tracecheck
	rm -f log/*.log log/*.proof log/*.trace log/*.err log/*.core
	rm -f *.bbg *.bb *.da *.gcov
	rm -f gmon.out
	rm -f cscope.out
	rm -f *~

booleforce: bfmain.o bfapp.o libbooleforce.a
	$(CC) $(CFLAGS) -o $@ bfmain.o bfapp.o -L. -lbooleforce

tracecheck: tcmain.o tracecheck.o libbooleforce.a
	$(CC) $(CFLAGS) -o $@ tcmain.o tracecheck.o -L. -lbooleforce

libbooleforce.a: $(OBJ)
	$(AR) rc $@ $(OBJ)
	$(RANLIB) $@

bfapp.o: config.h booleforce.h bftime.h bfio.h bfapp.c
bfio.o: config.h bfmem.h bfstack.h bfio.h bfio.c
bfmain.o: bfmain.c Makefile
bfmem.o: config.h booleforce.h bfmem.h bfmem.c
bfnum.o: config.h bfnum.h bfnum.c
bftime.o: config.h bftime.h bftime.c
booleforce.states: booleforce.c config.h states
booleforce.o: config.h bfmem.h bfstack.h bfsort.h \
  booleforce.h bftime.h bfio.h bfnum.h booleforce.states booleforce.c
tcmain.o: Makefile
test.o: config.h test.c
testapi.o: booleforce.h testapi.c
tracecheck.states: tracecheck.c config.h states
tracecheck.o: config.h bftime.h bfio.h bfmem.h \
  tracecheck.states tracecheck.c
