#pragma once
#include <sqlite3.h>

namespace db {
    class StmtGuard {
    public:
        explicit StmtGuard(sqlite3_stmt* stmt) : stmt_(stmt) {}
        ~StmtGuard() {
            if (stmt_) {
                sqlite3_finalize(stmt_);
            }
        }

        StmtGuard(const StmtGuard&) = delete;
        StmtGuard& operator=(const StmtGuard&) = delete;

    private:
        sqlite3_stmt* stmt_;
    };
}
