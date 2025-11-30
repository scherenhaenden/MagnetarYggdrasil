CC=gcc
CFLAGS=-Wall -Wextra -pedantic -g
LDFLAGS=-lkore -lsqlite3 -lm

SRC=src/main.c src/users.c src/tasks.c src/db.c src/helpers.c src/cJSON.c
OBJ=$(SRC:.c=.o)
TARGET=kore.so

all: $(TARGET)

$(TARGET): $(OBJ)
	$(CC) $(CFLAGS) -shared -fPIC -o $@ $^ $(LDFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) -fPIC -c -o $@ $<

clean:
	rm -f $(OBJ) $(TARGET)

test:
	$(CC) $(CFLAGS) -o tests/runner tests/test_runner.c $(LDFLAGS)
	./tests/runner
