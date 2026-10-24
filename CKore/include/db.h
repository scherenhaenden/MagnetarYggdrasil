#ifndef DB_H
#define DB_H

#include "models.h"
#include "cJSON.h"

// Initialize database (create tables)
void db_init(void);

// User operations
int db_create_user(const char *name, const char *email);
User *db_get_user(int id);
cJSON *db_get_all_users(void);
int db_update_user(int id, const char *name, const char *email);
int db_delete_user(int id);

// Task operations
int db_create_task(int user_id, const char *title, const char *description);
Task *db_get_task(int id);
cJSON *db_get_user_tasks(int user_id);
int db_update_task(int id, const char *title, const char *description);
int db_mark_task_done(int id);
int db_delete_task(int id);

#endif
