CC=gcc
CFLAGS=-Wall

all: ascii2shell

ascii2shell:
	$(CC) $(CFLAGS) -o ascii2shell ascii2shell.c

install:
	cp ascii2shell /usr/bin
	cp nasm2shell /usr/bin
	cp gas2shell /usr/bin
clean:
	rm -rf ascii2shell
