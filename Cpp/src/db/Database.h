#pragma once
#include <drogon/drogon.h>
#include <sqlite3.h>
#include <mutex>

namespace db {
    class Database {
    public:
        static Database& getInstance();
        void init();
        sqlite3* getDb();

    private:
        Database() = default;
        ~Database();
        sqlite3* db_ = nullptr;
        std::mutex mutex_;
    };
}
