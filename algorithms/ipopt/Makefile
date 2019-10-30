CC = gcc

CFLAGS  = -Wall -std=c11 $(shell pkg-config --cflags ipopt) $(shell pkg-config --cflags hdf5)
FAST_FLAGS  = -Wall -std=c11 -Ofast $(shell pkg-config --cflags ipopt) $(shell pkg-config --cflags hdf5)

LINK_LIBRARY = -lipopt -lm -lfftw3 -L/usr/lib/x86_64-linux-gnu/

all: ipopt.c problem.c utils.c $(SRC)
	$(CC) -o ipopt $(CFLAGS) ipopt.c $(SRC) $(LINK_LIBRARY)

fast: ipopt.c problem.c utils.c $(SRC)
	$(CC) -o ipopt $(FAST_FLAGS) ipopt.c $(SRC) $(LINK_LIBRARY)

clean:
	git clean -dfX

