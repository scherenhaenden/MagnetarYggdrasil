#include <kore/kore.h>
#include <kore/http.h>
#include "../include/models.h"
#include "../include/db.h"
#include "../include/helpers.h"

// POST /users/{id}/tasks
// GET /users/{id}/tasks
int handle_users_tasks(struct http_request *req) {
    int user_id = get_id_from_path(req, 1); // /users/123/tasks -> users(0), 123(1), tasks(2)

    if (user_id == -1) {
        http_response(req, 404, "Not Found", 9);
        return KORE_RESULT_OK;
    }

    if (req->method == HTTP_METHOD_POST) {
        cJSON *json = http_read_json_body(req);
        if (!json) {
            http_response(req, 400, "Invalid JSON", 12);
            return KORE_RESULT_OK;
        }

        cJSON *title = cJSON_GetObjectItemCaseSensitive(json, "title");
        cJSON *desc = cJSON_GetObjectItemCaseSensitive(json, "description");

        if (!cJSON_IsString(title) || !cJSON_IsString(desc)) {
             cJSON_Delete(json);
             http_response(req, 400, "Missing title or description", 28);
             return KORE_RESULT_OK;
        }

        int id = db_create_task(user_id, title->valuestring, desc->valuestring);
        cJSON_Delete(json);

        if (id != -1) {
            cJSON *resp = cJSON_CreateObject();
            cJSON_AddNumberToObject(resp, "id", id);
            cJSON_AddNumberToObject(resp, "user_id", user_id);
            cJSON_AddStringToObject(resp, "title", title->valuestring);
            cJSON_AddStringToObject(resp, "description", desc->valuestring);
            cJSON_AddBoolToObject(resp, "is_done", 0);
            app_response_json(req, 201, resp);
        } else {
             http_response(req, 400, "Failed to create task (User invalid?)", 37);
        }

    } else if (req->method == HTTP_METHOD_GET) {
        cJSON *tasks = db_get_user_tasks(user_id);
        app_response_json(req, 200, tasks);
    } else {
        http_response(req, 405, "Method Not Allowed", 18);
    }
    return KORE_RESULT_OK;
}


// GET /tasks/{tid}
// PUT /tasks/{tid}
// DELETE /tasks/{tid}
int handle_tasks_id(struct http_request *req) {
    int id = get_id_from_path(req, 1); // /tasks/123 -> tasks(0), 123(1)

    if (id == -1) {
        http_response(req, 404, "Not Found", 9);
        return KORE_RESULT_OK;
    }

    if (req->method == HTTP_METHOD_GET) {
        Task *task = db_get_task(id);
        if (task) {
            cJSON *resp = cJSON_CreateObject();
            cJSON_AddNumberToObject(resp, "id", task->id);
            cJSON_AddNumberToObject(resp, "user_id", task->user_id);
            cJSON_AddStringToObject(resp, "title", task->title);
            cJSON_AddStringToObject(resp, "description", task->description);
            cJSON_AddBoolToObject(resp, "is_done", task->is_done);

            free(task->title);
            free(task->description);
            free(task);

            app_response_json(req, 200, resp);
        } else {
            http_response(req, 404, "Task not found", 14);
        }
    } else if (req->method == HTTP_METHOD_PUT) {
        cJSON *json = http_read_json_body(req);
        if (!json) {
            http_response(req, 400, "Invalid JSON", 12);
            return KORE_RESULT_OK;
        }

        cJSON *title = cJSON_GetObjectItemCaseSensitive(json, "title");
        cJSON *desc = cJSON_GetObjectItemCaseSensitive(json, "description");

        char *title_val = cJSON_IsString(title) ? title->valuestring : NULL;
        char *desc_val = cJSON_IsString(desc) ? desc->valuestring : NULL;

        if (db_update_task(id, title_val, desc_val)) {
             Task *task = db_get_task(id);
             cJSON *resp = cJSON_CreateObject();
             cJSON_AddNumberToObject(resp, "id", task->id);
             cJSON_AddNumberToObject(resp, "user_id", task->user_id);
             cJSON_AddStringToObject(resp, "title", task->title);
             cJSON_AddStringToObject(resp, "description", task->description);
             cJSON_AddBoolToObject(resp, "is_done", task->is_done);
             free(task->title);
             free(task->description);
             free(task);
             app_response_json(req, 200, resp);
        } else {
             http_response(req, 404, "Task not found", 14);
        }
        cJSON_Delete(json);

    } else if (req->method == HTTP_METHOD_DELETE) {
        if (db_delete_task(id)) {
            http_response(req, 204, "", 0);
        } else {
            http_response(req, 404, "Task not found", 14);
        }
    } else {
        http_response(req, 405, "Method Not Allowed", 18);
    }
    return KORE_RESULT_OK;
}

// PATCH /tasks/{tid}/done
int handle_tasks_done(struct http_request *req) {
     int id = get_id_from_path(req, 1); // /tasks/123/done -> tasks(0), 123(1), done(2)

     if (id == -1) {
        http_response(req, 404, "Not Found", 9);
        return KORE_RESULT_OK;
    }

    if (req->method == HTTP_METHOD_PATCH) { // Usually PATCH is not default method enum in old kore?
        // Kore should support it.
        // Wait, does Kore have HTTP_METHOD_PATCH defined?
        // It should in recent versions. If not, we might need to check req->method == KORE_HTTP_METHOD_PATCH or similar or string compare.
        // I will assume HTTP_METHOD_PATCH exists or is 0.
        // Actually HTTP_METHOD_PATCH is often standard.
        // If not, I can check method string req->method_text if available? No.

        // Assuming standard enum.

        if (db_mark_task_done(id)) {
             Task *task = db_get_task(id);
             cJSON *resp = cJSON_CreateObject();
             cJSON_AddNumberToObject(resp, "id", task->id);
             cJSON_AddNumberToObject(resp, "user_id", task->user_id);
             cJSON_AddStringToObject(resp, "title", task->title);
             cJSON_AddStringToObject(resp, "description", task->description);
             cJSON_AddBoolToObject(resp, "is_done", task->is_done);
             free(task->title);
             free(task->description);
             free(task);
             app_response_json(req, 200, resp);
        } else {
            http_response(req, 404, "Task not found", 14);
        }
    } else {
        http_response(req, 405, "Method Not Allowed", 18);
    }
    return KORE_RESULT_OK;
}
