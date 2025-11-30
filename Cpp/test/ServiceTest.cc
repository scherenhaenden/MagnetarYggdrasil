#include <gtest/gtest.h>
#include "../src/services/UserService.h"
#include "../src/services/TaskService.h"
#include "../src/db/Database.h"

class ServiceTest : public ::testing::Test {
protected:
    void SetUp() override {
        sqlite3* db = db::Database::getInstance().getDb();
        sqlite3_exec(db, "DELETE FROM tasks;", 0, 0, 0);
        sqlite3_exec(db, "DELETE FROM users;", 0, 0, 0);
    }
};

TEST_F(ServiceTest, UserServiceFlow) {
    services::UserService userService;

    // Create
    auto user = userService.createUser("Dave", "dave@example.com");
    EXPECT_GT(user.id, 0);

    // Get
    auto retrieved = userService.getUserById(user.id);
    ASSERT_TRUE(retrieved.has_value());
    EXPECT_EQ(retrieved->email, "dave@example.com");

    // Update
    userService.updateUser(user.id, "David", "");
    auto updated = userService.getUserById(user.id);
    EXPECT_EQ(updated->name, "David");
    EXPECT_EQ(updated->email, "dave@example.com");

    // Delete
    EXPECT_TRUE(userService.deleteUser(user.id));
    EXPECT_FALSE(userService.getUserById(user.id).has_value());
}

TEST_F(ServiceTest, TaskServiceFlow) {
    services::UserService userService;
    services::TaskService taskService;

    auto user = userService.createUser("Eve", "eve@example.com");

    // Create Task
    auto task = taskService.createTask(user.id, "Work", "Hard");
    ASSERT_TRUE(task.has_value());
    EXPECT_EQ(task->user_id, user.id);

    // Get Tasks by User
    auto tasks = taskService.getTasksByUserId(user.id);
    ASSERT_TRUE(tasks.has_value());
    EXPECT_EQ(tasks->size(), 1);

    // Update Task
    taskService.updateTask(task->id, "Work Harder", "");
    auto updated = taskService.getTaskById(task->id);
    EXPECT_EQ(updated->title, "Work Harder");

    // Mark Done
    taskService.markTaskAsDone(task->id);
    updated = taskService.getTaskById(task->id);
    EXPECT_TRUE(updated->is_done);

    // Delete Task
    EXPECT_TRUE(taskService.deleteTask(task->id));
    EXPECT_FALSE(taskService.getTaskById(task->id).has_value());
}
