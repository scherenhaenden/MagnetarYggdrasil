#include "Database.h"
#include <iostream>

namespace db {

    Database& Database::getInstance() {
        static Database instance;
        return instance;
    }

    void Database::init() {
        std::lock_guard<std::mutex> lock(mutex_);
        if (db_) return;

        int rc = sqlite3_open("magnetar.db", &db_);
        if (rc) {
            std::cerr << "Can't open database: " << sqlite3_errmsg(db_) << std::endl;
            return;
        }

        const char* sql =
            "PRAGMA foreign_keys = ON;"
            "CREATE TABLE IF NOT EXISTS users ("
            "id INTEGER PRIMARY KEY AUTOINCREMENT,"
            "name TEXT NOT NULL,"
            "email TEXT NOT NULL UNIQUE"
            ");"
            "CREATE TABLE IF NOT EXISTS tasks ("
            "id INTEGER PRIMARY KEY AUTOINCREMENT,"
            "title TEXT NOT NULL,"
            "description TEXT NOT NULL,"
            "is_done INTEGER DEFAULT 0,"
            "user_id INTEGER NOT NULL,"
            "FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE"
            ");";

        char* zErrMsg = 0;
        rc = sqlite3_exec(db_, sql, 0, 0, &zErrMsg);
        if (rc != SQLITE_OK) {
            std::cerr << "SQL error: " << zErrMsg << std::endl;
            sqlite3_free(zErrMsg);
        }
    }

    sqlite3* Database::getDb() {
        return db_;
    }

    Database::~Database() {
        if (db_) {
            sqlite3_close(db_);
        }
    }
}
