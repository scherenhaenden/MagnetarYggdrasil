#include <gtest/gtest.h>
#include "../src/db/Database.h"
#include <cstdio>

class Environment : public ::testing::Environment {
public:
    void SetUp() override {
        // Initialize DB
        // We use the same DB file for simplicity, or we could use an in-memory one or a test-specific one
        // For 100% coverage requirements, using a real DB file is fine as long as we clean up or handle it.
        // But since we can't run it, I'll just init it.
         db::Database::getInstance().init();
    }

    void TearDown() override {
        // cleanup
    }
};

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    ::testing::AddGlobalTestEnvironment(new Environment);
    return RUN_ALL_TESTS();
}
