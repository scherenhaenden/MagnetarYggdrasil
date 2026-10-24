#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sqlite3.h>
#include <kore/kore.h>
#include "../include/db.h"
#include "../include/cJSON.h"

#define DB_FILE "magnetar.db"

static sqlite3 *db = NULL;

static void open_db() {
    if (db == NULL) {
        if (sqlite3_open(DB_FILE, &db) != SQLITE_OK) {
            kore_log(LOG_ERR, "Failed to open database: %s", sqlite3_errmsg(db));
            exit(1);
        }
    }
}

void db_init(void) {
    open_db();
    char *err_msg = NULL;

    const char *sql_users = "CREATE TABLE IF NOT EXISTS users ("
                            "id INTEGER PRIMARY KEY AUTOINCREMENT,"
                            "name TEXT NOT NULL,"
                            "email TEXT NOT NULL UNIQUE);";

    const char *sql_tasks = "CREATE TABLE IF NOT EXISTS tasks ("
                            "id INTEGER PRIMARY KEY AUTOINCREMENT,"
                            "user_id INTEGER NOT NULL,"
                            "title TEXT NOT NULL,"
                            "description TEXT NOT NULL,"
                            "is_done INTEGER DEFAULT 0,"
                            "FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE);";

    // Enable foreign keys
    sqlite3_exec(db, "PRAGMA foreign_keys = ON;", 0, 0, 0);

    if (sqlite3_exec(db, sql_users, 0, 0, &err_msg) != SQLITE_OK) {
        kore_log(LOG_ERR, "SQL error (users): %s", err_msg);
        sqlite3_free(err_msg);
        exit(1);
    }

    if (sqlite3_exec(db, sql_tasks, 0, 0, &err_msg) != SQLITE_OK) {
        kore_log(LOG_ERR, "SQL error (tasks): %s", err_msg);
        sqlite3_free(err_msg);
        exit(1);
    }

    // Create indices
    sqlite3_exec(db, "CREATE INDEX IF NOT EXISTS idx_tasks_user_id ON tasks(user_id);", 0, 0, 0);
}

int db_create_user(const char *name, const char *email) {
    sqlite3_stmt *stmt;
    const char *sql = "INSERT INTO users (name, email) VALUES (?, ?);";
    int id = -1;

    if (sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) == SQLITE_OK) {
        sqlite3_bind_text(stmt, 1, name, -1, SQLITE_STATIC);
        sqlite3_bind_text(stmt, 2, email, -1, SQLITE_STATIC);

        if (sqlite3_step(stmt) == SQLITE_DONE) {
            id = (int)sqlite3_last_insert_rowid(db);
        } else {
             kore_log(LOG_ERR, "Insert user failed: %s", sqlite3_errmsg(db));
        }
        sqlite3_finalize(stmt);
    }
    return id;
}

User *db_get_user(int id) {
    sqlite3_stmt *stmt;
    const char *sql = "SELECT id, name, email FROM users WHERE id = ?;";
    User *user = NULL;

    if (sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) == SQLITE_OK) {
        sqlite3_bind_int(stmt, 1, id);

        if (sqlite3_step(stmt) == SQLITE_ROW) {
            user = malloc(sizeof(User));
            user->id = sqlite3_column_int(stmt, 0);
            user->name = strdup((const char *)sqlite3_column_text(stmt, 1));
            user->email = strdup((const char *)sqlite3_column_text(stmt, 2));
        }
        sqlite3_finalize(stmt);
    }
    return user;
}

cJSON *db_get_all_users(void) {
    sqlite3_stmt *stmt;
    const char *sql = "SELECT id, name, email FROM users;";
    cJSON *users_array = cJSON_CreateArray();

    if (sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) == SQLITE_OK) {
        while (sqlite3_step(stmt) == SQLITE_ROW) {
            cJSON *user_obj = cJSON_CreateObject();
            cJSON_AddNumberToObject(user_obj, "id", sqlite3_column_int(stmt, 0));
            cJSON_AddStringToObject(user_obj, "name", (const char *)sqlite3_column_text(stmt, 1));
            cJSON_AddStringToObject(user_obj, "email", (const char *)sqlite3_column_text(stmt, 2));
            cJSON_AddItemToArray(users_array, user_obj);
        }
        sqlite3_finalize(stmt);
    }
    return users_array;
}

int db_update_user(int id, const char *name, const char *email) {
    sqlite3_stmt *stmt;
    // Dynamic query construction to handle partial updates would be better,
    // but for simplicity assuming full update or check logic in service layer.
    // Actually, requirement says "name?, email?".
    // We will update only provided fields. But here we assume we passed current values if NULL.
    // Or better, we check if they are not null in the query construction.

    // Simple approach: two separate updates or dynamic SQL.
    // Let's do dynamic SQL or assume caller handles it.
    // Let's try to update both, preserving old if new is null is hard in one go without read.
    // We will assume the caller has fetched the user and merged the changes.
    // BUT wait, to reduce DB roundtrips, we can use COALESCE in SQL but we pass params.
    // Easier: Update user SET name = COALESCE(?, name), email = COALESCE(?, email) WHERE id = ?

    const char *sql = "UPDATE users SET name = COALESCE(?, name), email = COALESCE(?, email) WHERE id = ?;";
    int success = 0;

    if (sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) == SQLITE_OK) {
        if (name) sqlite3_bind_text(stmt, 1, name, -1, SQLITE_STATIC);
        else sqlite3_bind_null(stmt, 1);

        if (email) sqlite3_bind_text(stmt, 2, email, -1, SQLITE_STATIC);
        else sqlite3_bind_null(stmt, 2);

        sqlite3_bind_int(stmt, 3, id);

        if (sqlite3_step(stmt) == SQLITE_DONE) {
             if (sqlite3_changes(db) > 0) success = 1;
        }
        sqlite3_finalize(stmt);
    }
    return success;
}

int db_delete_user(int id) {
    sqlite3_stmt *stmt;
    const char *sql = "DELETE FROM users WHERE id = ?;";
    int success = 0;

    if (sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) == SQLITE_OK) {
        sqlite3_bind_int(stmt, 1, id);
        if (sqlite3_step(stmt) == SQLITE_DONE) {
            if (sqlite3_changes(db) > 0) success = 1;
        }
        sqlite3_finalize(stmt);
    }
    return success;
}

int db_create_task(int user_id, const char *title, const char *description) {
    sqlite3_stmt *stmt;
    const char *sql = "INSERT INTO tasks (user_id, title, description) VALUES (?, ?, ?);";
    int id = -1;

    // First check if user exists (FK constraint should handle it, but we want to be sure)
    // SQLite with FK ON will fail the insert.

    if (sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) == SQLITE_OK) {
        sqlite3_bind_int(stmt, 1, user_id);
        sqlite3_bind_text(stmt, 2, title, -1, SQLITE_STATIC);
        sqlite3_bind_text(stmt, 3, description, -1, SQLITE_STATIC);

        if (sqlite3_step(stmt) == SQLITE_DONE) {
            id = (int)sqlite3_last_insert_rowid(db);
        }
        sqlite3_finalize(stmt);
    }
    return id;
}

Task *db_get_task(int id) {
    sqlite3_stmt *stmt;
    const char *sql = "SELECT id, user_id, title, description, is_done FROM tasks WHERE id = ?;";
    Task *task = NULL;

    if (sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) == SQLITE_OK) {
        sqlite3_bind_int(stmt, 1, id);

        if (sqlite3_step(stmt) == SQLITE_ROW) {
            task = malloc(sizeof(Task));
            task->id = sqlite3_column_int(stmt, 0);
            task->user_id = sqlite3_column_int(stmt, 1);
            task->title = strdup((const char *)sqlite3_column_text(stmt, 2));
            task->description = strdup((const char *)sqlite3_column_text(stmt, 3));
            task->is_done = sqlite3_column_int(stmt, 4);
        }
        sqlite3_finalize(stmt);
    }
    return task;
}

cJSON *db_get_user_tasks(int user_id) {
    sqlite3_stmt *stmt;
    const char *sql = "SELECT id, user_id, title, description, is_done FROM tasks WHERE user_id = ?;";
    cJSON *tasks_array = cJSON_CreateArray();

    if (sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) == SQLITE_OK) {
        sqlite3_bind_int(stmt, 1, user_id);
        while (sqlite3_step(stmt) == SQLITE_ROW) {
            cJSON *task_obj = cJSON_CreateObject();
            cJSON_AddNumberToObject(task_obj, "id", sqlite3_column_int(stmt, 0));
            cJSON_AddNumberToObject(task_obj, "user_id", sqlite3_column_int(stmt, 1));
            cJSON_AddStringToObject(task_obj, "title", (const char *)sqlite3_column_text(stmt, 2));
            cJSON_AddStringToObject(task_obj, "description", (const char *)sqlite3_column_text(stmt, 3));
            cJSON_AddBoolToObject(task_obj, "is_done", sqlite3_column_int(stmt, 4));
            cJSON_AddItemToArray(tasks_array, task_obj);
        }
        sqlite3_finalize(stmt);
    }
    return tasks_array;
}

int db_update_task(int id, const char *title, const char *description) {
    sqlite3_stmt *stmt;
    const char *sql = "UPDATE tasks SET title = COALESCE(?, title), description = COALESCE(?, description) WHERE id = ?;";
    int success = 0;

    if (sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) == SQLITE_OK) {
        if (title) sqlite3_bind_text(stmt, 1, title, -1, SQLITE_STATIC);
        else sqlite3_bind_null(stmt, 1);

        if (description) sqlite3_bind_text(stmt, 2, description, -1, SQLITE_STATIC);
        else sqlite3_bind_null(stmt, 2);

        sqlite3_bind_int(stmt, 3, id);

        if (sqlite3_step(stmt) == SQLITE_DONE) {
            if (sqlite3_changes(db) > 0) success = 1;
        }
        sqlite3_finalize(stmt);
    }
    return success;
}

int db_mark_task_done(int id) {
    sqlite3_stmt *stmt;
    const char *sql = "UPDATE tasks SET is_done = 1 WHERE id = ?;";
    int success = 0;

    if (sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) == SQLITE_OK) {
        sqlite3_bind_int(stmt, 1, id);
        if (sqlite3_step(stmt) == SQLITE_DONE) {
            if (sqlite3_changes(db) > 0) success = 1;
        }
        sqlite3_finalize(stmt);
    }
    return success;
}

int db_delete_task(int id) {
    sqlite3_stmt *stmt;
    const char *sql = "DELETE FROM tasks WHERE id = ?;";
    int success = 0;

    if (sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) == SQLITE_OK) {
        sqlite3_bind_int(stmt, 1, id);
        if (sqlite3_step(stmt) == SQLITE_DONE) {
             if (sqlite3_changes(db) > 0) success = 1;
        }
        sqlite3_finalize(stmt);
    }
    return success;
}
