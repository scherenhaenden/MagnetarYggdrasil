#ifndef HELPERS_H
#define HELPERS_H

#include <kore/kore.h>
#include <kore/http.h>
#include "cJSON.h"

// Helper to send JSON response
void http_response_json(struct http_request *req, int status, cJSON *json);

// Helper to read JSON body
cJSON *http_read_json_body(struct http_request *req);

// Extract ID from path (e.g., /users/123 -> 123)
int get_id_from_path(struct http_request *req, int index);

#endif
