#ifndef KORE_KORE_H
#define KORE_KORE_H

#include <stddef.h>

/* Minimal stub definitions to allow building without the real Kore library. */
#define KORE_RESULT_OK 0
#define KORE_RESULT_ERROR -1

/* Logging levels (subset used in this project). */
#define LOG_ERR 3

struct kore_buf {
    void *data;
    size_t offset;
};

void kore_log(int level, const char *fmt, ...);

#endif
