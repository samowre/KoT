CC=gcc
CFLAGS= -DSTATS -DTRACE -Wall -Wextra -DNDEBUG -O2

all: picosat libpicosat.a

clean:
	rm -f picosat *.exe *.s *.o *.a *.so
	rm -f makefile config.h
	rm -f gmon.out *~ 

picosat: libpicosat.a app.o main.o
	$(CC) $(CFLAGS) -o $@ main.o app.o -L. -lpicosat

app.o: app.c picosat.h makefile
	$(CC) $(CFLAGS) -c $<

main.o: main.c picosat.h makefile
	$(CC) $(CFLAGS) -c $<

picosat.o: picosat.c picosat.h makefile
	$(CC) $(CFLAGS) -c $<

version.o: version.c config.h makefile
	$(CC) $(CFLAGS) -c $<

config.h: makefile VERSION mkconfig # and actually picosat.c
	rm -f $@; ./mkconfig > $@

libpicosat.a: picosat.o version.o
	ar rc $@ picosat.o version.o
	ranlib $@

.PHONY: all clean
