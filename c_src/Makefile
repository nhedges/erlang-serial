#makefile

CC = g++

all: ../priv/bin/serial

../priv/bin/serial: serial ../priv/bin
	install -m 755 serial ../priv/bin

serial: serial.c serial.h
	$(CC) -o serial serial.c

../priv/bin:
	mkdir -p ../priv/bin
