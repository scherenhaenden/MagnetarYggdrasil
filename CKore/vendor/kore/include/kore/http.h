#ifndef KORE_HTTP_H
#define KORE_HTTP_H

#include "kore.h"

/* HTTP method constants mirroring Kore's enum values. */
#define HTTP_METHOD_GET    0
#define HTTP_METHOD_POST   1
#define HTTP_METHOD_PUT    2
#define HTTP_METHOD_DELETE 3
#define HTTP_METHOD_PATCH  4

struct http_request {
    int method;
    struct kore_buf *http_body;
    char *path;
};

void http_response(struct http_request *req, int status, const void *data, size_t len);

#endif
