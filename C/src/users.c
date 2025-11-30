#include <kore/kore.h>
#include <kore/http.h>
#include "../include/models.h"
#include "../include/db.h"
#include "../include/helpers.h"

// POST /users
// GET /users
int handle_users(struct http_request *req) {
    if (req->method == HTTP_METHOD_POST) {
        cJSON *json = http_read_json_body(req);
        if (!json) {
            http_response(req, 400, "Invalid JSON", 12);
            return KORE_RESULT_OK;
        }

        cJSON *name = cJSON_GetObjectItemCaseSensitive(json, "name");
        cJSON *email = cJSON_GetObjectItemCaseSensitive(json, "email");

        if (!cJSON_IsString(name) || !cJSON_IsString(email)) {
             cJSON_Delete(json);
             http_response(req, 400, "Missing name or email", 21);
             return KORE_RESULT_OK;
        }

        int id = db_create_user(name->valuestring, email->valuestring);
        cJSON_Delete(json);

        if (id != -1) {
            cJSON *resp = cJSON_CreateObject();
            cJSON_AddNumberToObject(resp, "id", id);
            cJSON_AddStringToObject(resp, "name", name->valuestring);
            cJSON_AddStringToObject(resp, "email", email->valuestring);
            app_response_json(req, 201, resp);
        } else {
             http_response(req, 409, "User already exists or error", 28);
        }
    } else if (req->method == HTTP_METHOD_GET) {
        cJSON *users = db_get_all_users();
        app_response_json(req, 200, users);
    } else {
        http_response(req, 405, "Method Not Allowed", 18);
    }
    return KORE_RESULT_OK;
}

// GET /users/{id}
// PUT /users/{id}
// DELETE /users/{id}
int handle_users_id(struct http_request *req) {
    int id = get_id_from_path(req, 1); // /users/123 -> users(0), 123(1)

    if (id == -1) {
        http_response(req, 404, "Not Found", 9);
        return KORE_RESULT_OK;
    }

    if (req->method == HTTP_METHOD_GET) {
        User *user = db_get_user(id);
        if (user) {
            cJSON *resp = cJSON_CreateObject();
            cJSON_AddNumberToObject(resp, "id", user->id);
            cJSON_AddStringToObject(resp, "name", user->name);
            cJSON_AddStringToObject(resp, "email", user->email);

            free(user->name);
            free(user->email);
            free(user);

            app_response_json(req, 200, resp);
        } else {
            http_response(req, 404, "User not found", 14);
        }
    } else if (req->method == HTTP_METHOD_PUT) {
        cJSON *json = http_read_json_body(req);
        if (!json) {
            http_response(req, 400, "Invalid JSON", 12);
            return KORE_RESULT_OK;
        }

        cJSON *name = cJSON_GetObjectItemCaseSensitive(json, "name");
        cJSON *email = cJSON_GetObjectItemCaseSensitive(json, "email");

        // name and email are optional in PUT? Requirement says "name?, email?".
        // If neither, what do we do? We just update nothing and return OK?
        // Or if fields are present, they are strings.

        char *name_val = cJSON_IsString(name) ? name->valuestring : NULL;
        char *email_val = cJSON_IsString(email) ? email->valuestring : NULL;

        if (db_update_user(id, name_val, email_val)) {
             // Return updated user
             User *user = db_get_user(id);
             cJSON *resp = cJSON_CreateObject();
             cJSON_AddNumberToObject(resp, "id", user->id);
             cJSON_AddStringToObject(resp, "name", user->name);
             cJSON_AddStringToObject(resp, "email", user->email);
             free(user->name);
             free(user->email);
             free(user);
             app_response_json(req, 200, resp);
        } else {
             http_response(req, 404, "User not found or nothing to update", 33);
        }
        cJSON_Delete(json);
    } else if (req->method == HTTP_METHOD_DELETE) {
        if (db_delete_user(id)) {
            http_response(req, 204, "", 0);
        } else {
            http_response(req, 404, "User not found", 14);
        }
    } else {
        http_response(req, 405, "Method Not Allowed", 18);
    }
    return KORE_RESULT_OK;
}
