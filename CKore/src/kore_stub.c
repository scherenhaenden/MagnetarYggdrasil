#include <stdarg.h>
#include <stdio.h>
#include "../vendor/kore/include/kore/http.h"

/*
 * Stub implementation of Kore HTTP helpers used by the project. The goal is
 * simply to satisfy the build and allow unit tests that do not exercise the
 * real web server to link successfully.
 */
void http_response(struct http_request *req, int status, const void *data, size_t len) {
    (void)req;
    (void)status;
    (void)data;
    (void)len;
}

void kore_log(int level, const char *fmt, ...) {
    (void)level;
    (void)fmt;

    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fputc('\n', stderr);
}
