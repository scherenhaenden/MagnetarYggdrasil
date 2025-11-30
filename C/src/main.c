#include <kore/kore.h>
#include <kore/http.h>
#include "../include/db.h"

int init(int state);
int handle_health(struct http_request *req);

int init(int state) {
    db_init();
    return KORE_RESULT_OK;
}

int handle_health(struct http_request *req) {
    http_response(req, 200, "OK", 2);
    return KORE_RESULT_OK;
}
