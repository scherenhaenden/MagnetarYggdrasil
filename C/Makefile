CC=gcc
CFLAGS=-Wall -Wextra -pedantic -g
LDFLAGS=-lsqlite3 -lm

# Try to find kore using pkg-config or standard paths
KORE_CFLAGS=$(shell pkg-config --cflags kore 2>/dev/null)
KORE_LDFLAGS=$(shell pkg-config --libs kore 2>/dev/null)

ifeq ($(strip $(KORE_CFLAGS)),)
    # If pkg-config fails, check vendor directory
    ifneq ($(wildcard vendor/kore/include/kore/kore.h),)
        CFLAGS += -Ivendor/kore/include
    else
        # Fallback to standard locations (e.g. /usr/local/include)
        CFLAGS += -I/usr/local/include
        LDFLAGS += -L/usr/local/lib -lkore
    endif
else
    CFLAGS += $(KORE_CFLAGS)
    LDFLAGS += $(KORE_LDFLAGS)
endif

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
