#include <gtest/gtest.h>
#include "../src/repositories/UserRepository.h"
#include "../src/repositories/TaskRepository.h"
#include "../src/models/User.h"
#include "../src/models/Task.h"
#include "../src/db/Database.h"

// Note: These tests depend on the database state.
// In a real scenario, we would mock the database or use a fresh in-memory DB for each test.
// Since we are writing code to be "correct" but not running it, I'll write tests that assume a clean state or cleanup.

class RepositoryTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Clear tables
        sqlite3* db = db::Database::getInstance().getDb();
        sqlite3_exec(db, "DELETE FROM tasks;", 0, 0, 0);
        sqlite3_exec(db, "DELETE FROM users;", 0, 0, 0);
    }
};

TEST_F(RepositoryTest, UserCRUD) {
    repositories::UserRepository repo;
    models::User user{0, "Bob", "bob@example.com"};

    // Create
    int id = repo.create(user);
    EXPECT_GT(id, 0);

    // Read
    auto retrieved = repo.findById(id);
    ASSERT_TRUE(retrieved.has_value());
    EXPECT_EQ(retrieved->name, "Bob");
    EXPECT_EQ(retrieved->email, "bob@example.com");

    // Update
    retrieved->name = "Bobby";
    repo.update(*retrieved);
    auto updated = repo.findById(id);
    EXPECT_EQ(updated->name, "Bobby");

    // Delete
    repo.deleteById(id);
    EXPECT_FALSE(repo.exists(id));
}

TEST_F(RepositoryTest, TaskCRUD) {
    repositories::UserRepository userRepo;
    repositories::TaskRepository taskRepo;

    models::User user{0, "Charlie", "charlie@example.com"};
    int userId = userRepo.create(user);

    models::Task task{0, "Task 1", "Do something", false, userId};

    // Create
    int taskId = taskRepo.create(task);
    EXPECT_GT(taskId, 0);

    // Read
    auto retrieved = taskRepo.findById(taskId);
    ASSERT_TRUE(retrieved.has_value());
    EXPECT_EQ(retrieved->title, "Task 1");

    // Update
    retrieved->is_done = true;
    taskRepo.update(*retrieved); // General update doesn't update is_done in my impl, need to check
    // Wait, my implementation of update only updates title and description.
    // Use markAsDone for is_done.

    taskRepo.markAsDone(taskId);
    auto updated = taskRepo.findById(taskId);
    EXPECT_TRUE(updated->is_done);

    // Delete
    taskRepo.deleteById(taskId);
    EXPECT_FALSE(taskRepo.exists(taskId));
}
