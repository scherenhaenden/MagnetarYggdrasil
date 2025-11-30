#include "UserRepository.h"
#include "../db/Database.h"
#include "../db/StmtGuard.h"
#include <iostream>

namespace repositories {

    int UserRepository::create(const models::User& user) {
        sqlite3* db = db::Database::getInstance().getDb();
        sqlite3_stmt* stmt;
        const char* sql = "INSERT INTO users (name, email) VALUES (?, ?);";

        if (sqlite3_prepare_v2(db, sql, -1, &stmt, 0) != SQLITE_OK) {
             throw std::runtime_error("Failed to prepare statement");
        }
        db::StmtGuard guard(stmt);

        sqlite3_bind_text(stmt, 1, user.name.c_str(), -1, SQLITE_STATIC);
        sqlite3_bind_text(stmt, 2, user.email.c_str(), -1, SQLITE_STATIC);

        if (sqlite3_step(stmt) != SQLITE_DONE) {
            throw std::runtime_error("Failed to execute statement");
        }

        return (int)sqlite3_last_insert_rowid(db);
    }

    std::optional<models::User> UserRepository::findById(int id) {
        sqlite3* db = db::Database::getInstance().getDb();
        sqlite3_stmt* stmt;
        const char* sql = "SELECT id, name, email FROM users WHERE id = ?;";

        if (sqlite3_prepare_v2(db, sql, -1, &stmt, 0) != SQLITE_OK) {
             throw std::runtime_error("Failed to prepare statement");
        }
        db::StmtGuard guard(stmt);

        sqlite3_bind_int(stmt, 1, id);

        if (sqlite3_step(stmt) == SQLITE_ROW) {
            models::User user;
            user.id = sqlite3_column_int(stmt, 0);
            user.name = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 1));
            user.email = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 2));
            return user;
        }

        return std::nullopt;
    }

    std::vector<models::User> UserRepository::findAll() {
        std::vector<models::User> users;
        sqlite3* db = db::Database::getInstance().getDb();
        sqlite3_stmt* stmt;
        const char* sql = "SELECT id, name, email FROM users;";

        if (sqlite3_prepare_v2(db, sql, -1, &stmt, 0) != SQLITE_OK) {
             throw std::runtime_error("Failed to prepare statement");
        }
        db::StmtGuard guard(stmt);

        while (sqlite3_step(stmt) == SQLITE_ROW) {
            models::User user;
            user.id = sqlite3_column_int(stmt, 0);
            user.name = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 1));
            user.email = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 2));
            users.push_back(user);
        }

        return users;
    }

    void UserRepository::update(const models::User& user) {
        sqlite3* db = db::Database::getInstance().getDb();

        std::string sql = "UPDATE users SET ";
        bool first = true;
        bool hasUpdate = false;

        if (!user.name.empty()) {
            sql += "name = ?";
            first = false;
            hasUpdate = true;
        }
        if (!user.email.empty()) {
            if (!first) sql += ", ";
            sql += "email = ?";
            hasUpdate = true;
        }

        if (!hasUpdate) return; // Nothing to update

        sql += " WHERE id = ?;";

        sqlite3_stmt* stmt;
        if (sqlite3_prepare_v2(db, sql.c_str(), -1, &stmt, 0) != SQLITE_OK) {
             throw std::runtime_error("Failed to prepare statement");
        }
        db::StmtGuard guard(stmt);

        int index = 1;
        if (!user.name.empty()) {
            sqlite3_bind_text(stmt, index++, user.name.c_str(), -1, SQLITE_STATIC);
        }
        if (!user.email.empty()) {
            sqlite3_bind_text(stmt, index++, user.email.c_str(), -1, SQLITE_STATIC);
        }
        sqlite3_bind_int(stmt, index, user.id);

        if (sqlite3_step(stmt) != SQLITE_DONE) {
            throw std::runtime_error("Failed to execute statement");
        }
    }

    void UserRepository::deleteById(int id) {
        sqlite3* db = db::Database::getInstance().getDb();
        sqlite3_stmt* stmt;
        const char* sql = "DELETE FROM users WHERE id = ?;";

        if (sqlite3_prepare_v2(db, sql, -1, &stmt, 0) != SQLITE_OK) {
             throw std::runtime_error("Failed to prepare statement");
        }
        db::StmtGuard guard(stmt);

        sqlite3_bind_int(stmt, 1, id);

        if (sqlite3_step(stmt) != SQLITE_DONE) {
            throw std::runtime_error("Failed to execute statement");
        }
    }

    bool UserRepository::exists(int id) {
        return findById(id).has_value();
    }
}
