#include "TaskRepository.h"
#include "../db/Database.h"
#include "../db/StmtGuard.h"
#include <iostream>

namespace repositories {

    int TaskRepository::create(const models::Task& task) {
        sqlite3* db = db::Database::getInstance().getDb();
        sqlite3_stmt* stmt;
        const char* sql = "INSERT INTO tasks (title, description, is_done, user_id) VALUES (?, ?, ?, ?);";

        if (sqlite3_prepare_v2(db, sql, -1, &stmt, 0) != SQLITE_OK) {
             throw std::runtime_error("Failed to prepare statement");
        }
        db::StmtGuard guard(stmt);

        sqlite3_bind_text(stmt, 1, task.title.c_str(), -1, SQLITE_STATIC);
        sqlite3_bind_text(stmt, 2, task.description.c_str(), -1, SQLITE_STATIC);
        sqlite3_bind_int(stmt, 3, task.is_done ? 1 : 0);
        sqlite3_bind_int(stmt, 4, task.user_id);

        if (sqlite3_step(stmt) != SQLITE_DONE) {
            throw std::runtime_error("Failed to execute statement");
        }

        return (int)sqlite3_last_insert_rowid(db);
    }

    std::optional<models::Task> TaskRepository::findById(int id) {
        sqlite3* db = db::Database::getInstance().getDb();
        sqlite3_stmt* stmt;
        const char* sql = "SELECT id, title, description, is_done, user_id FROM tasks WHERE id = ?;";

        if (sqlite3_prepare_v2(db, sql, -1, &stmt, 0) != SQLITE_OK) {
             throw std::runtime_error("Failed to prepare statement");
        }
        db::StmtGuard guard(stmt);

        sqlite3_bind_int(stmt, 1, id);

        if (sqlite3_step(stmt) == SQLITE_ROW) {
            models::Task task;
            task.id = sqlite3_column_int(stmt, 0);
            task.title = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 1));
            task.description = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 2));
            task.is_done = sqlite3_column_int(stmt, 3) != 0;
            task.user_id = sqlite3_column_int(stmt, 4);
            return task;
        }

        return std::nullopt;
    }

    std::vector<models::Task> TaskRepository::findAllByUserId(int userId) {
        std::vector<models::Task> tasks;
        sqlite3* db = db::Database::getInstance().getDb();
        sqlite3_stmt* stmt;
        const char* sql = "SELECT id, title, description, is_done, user_id FROM tasks WHERE user_id = ?;";

        if (sqlite3_prepare_v2(db, sql, -1, &stmt, 0) != SQLITE_OK) {
             throw std::runtime_error("Failed to prepare statement");
        }
        db::StmtGuard guard(stmt);

        sqlite3_bind_int(stmt, 1, userId);

        while (sqlite3_step(stmt) == SQLITE_ROW) {
            models::Task task;
            task.id = sqlite3_column_int(stmt, 0);
            task.title = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 1));
            task.description = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 2));
            task.is_done = sqlite3_column_int(stmt, 3) != 0;
            task.user_id = sqlite3_column_int(stmt, 4);
            tasks.push_back(task);
        }

        return tasks;
    }

    void TaskRepository::update(const models::Task& task) {
        sqlite3* db = db::Database::getInstance().getDb();

        std::string sql = "UPDATE tasks SET ";
        bool first = true;
        bool hasUpdate = false;

        if (!task.title.empty()) {
            sql += "title = ?";
            first = false;
            hasUpdate = true;
        }
        if (!task.description.empty()) {
            if (!first) sql += ", ";
            sql += "description = ?";
            hasUpdate = true;
        }

        if (!hasUpdate) return;

        sql += " WHERE id = ?;";

        sqlite3_stmt* stmt;
        if (sqlite3_prepare_v2(db, sql.c_str(), -1, &stmt, 0) != SQLITE_OK) {
             throw std::runtime_error("Failed to prepare statement");
        }
        db::StmtGuard guard(stmt);

        int index = 1;
        if (!task.title.empty()) {
            sqlite3_bind_text(stmt, index++, task.title.c_str(), -1, SQLITE_STATIC);
        }
        if (!task.description.empty()) {
            sqlite3_bind_text(stmt, index++, task.description.c_str(), -1, SQLITE_STATIC);
        }
        sqlite3_bind_int(stmt, index, task.id);

        if (sqlite3_step(stmt) != SQLITE_DONE) {
            throw std::runtime_error("Failed to execute statement");
        }
    }

    void TaskRepository::markAsDone(int id) {
        sqlite3* db = db::Database::getInstance().getDb();
        sqlite3_stmt* stmt;
        const char* sql = "UPDATE tasks SET is_done = 1 WHERE id = ?;";

        if (sqlite3_prepare_v2(db, sql, -1, &stmt, 0) != SQLITE_OK) {
             throw std::runtime_error("Failed to prepare statement");
        }
        db::StmtGuard guard(stmt);

        sqlite3_bind_int(stmt, 1, id);

        if (sqlite3_step(stmt) != SQLITE_DONE) {
            throw std::runtime_error("Failed to execute statement");
        }
    }

    void TaskRepository::deleteById(int id) {
        sqlite3* db = db::Database::getInstance().getDb();
        sqlite3_stmt* stmt;
        const char* sql = "DELETE FROM tasks WHERE id = ?;";

        if (sqlite3_prepare_v2(db, sql, -1, &stmt, 0) != SQLITE_OK) {
             throw std::runtime_error("Failed to prepare statement");
        }
        db::StmtGuard guard(stmt);

        sqlite3_bind_int(stmt, 1, id);

        if (sqlite3_step(stmt) != SQLITE_DONE) {
            throw std::runtime_error("Failed to execute statement");
        }
    }

    bool TaskRepository::exists(int id) {
        return findById(id).has_value();
    }
}
