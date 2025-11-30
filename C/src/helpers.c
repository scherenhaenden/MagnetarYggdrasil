#include "../include/helpers.h"
#include <stdlib.h>
#include <string.h>

void http_response_json(struct http_request *req, int status, cJSON *json) {
    char *json_str = cJSON_PrintUnformatted(json);
    http_response(req, status, json_str, strlen(json_str));
    free(json_str);
    cJSON_Delete(json);
}

cJSON *http_read_json_body(struct http_request *req) {
    if (req->http_body == NULL) return NULL;

    // In Kore, http_body is a struct kore_buf *
    // http_body_length in req is the Content-Length.
    // http_body->offset is the actual length of data in the buffer.

    // Use the buffer offset (actual data length)
    size_t len = req->http_body->offset;

    if (len == 0) return NULL;

    char *body_str = malloc(len + 1);
    if (!body_str) return NULL;

    memcpy(body_str, req->http_body->data, len);
    body_str[len] = '\0';

    cJSON *json = cJSON_Parse(body_str);
    free(body_str);
    return json;
}

int get_id_from_path(struct http_request *req, int index) {
    char *path_copy = strdup(req->path);
    char *saveptr;
    char *token = strtok_r(path_copy, "/", &saveptr);

    char *val = NULL;
    int i = 0;
    while (token != NULL) {
        if (i == index) {
            val = token;
            break;
        }
        token = strtok_r(NULL, "/", &saveptr);
        i++;
    }

    int id = -1;
    if (val) {
        id = atoi(val);
    }

    free(path_copy);
    return id;
}
