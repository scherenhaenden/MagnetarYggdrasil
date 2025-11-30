const std = @import("std");
const testing = std.testing;
const DB = @import("db.zig").DB;
const Repository = @import("repository.zig").Repository;
const Service = @import("service.zig").Service;
const models = @import("models.zig");

// Integration tests for Repository and Service layers
test "Integration Test: User Flow" {
    const allocator = testing.allocator;

    // Use a unique DB file for testing to avoid conflicts
    var db = try DB.init(allocator, "test_users.db");
    defer {
        db.deinit();
        std.fs.cwd().deleteFile("test_users.db") catch {};
        std.fs.cwd().deleteFile("test_users.db-shm") catch {};
        std.fs.cwd().deleteFile("test_users.db-wal") catch {};
    }

    var repo = Repository.init(&db);
    var service = Service.init(&repo, allocator);

    // 1. Create User
    const user = try service.createUser("Alice", "alice@example.com");
    try testing.expect(user.id > 0);
    try testing.expectEqualStrings("Alice", user.name);
    try testing.expectEqualStrings("alice@example.com", user.email);

    // 2. Get User
    const fetched_user = try service.getUser(user.id);
    defer {
        allocator.free(fetched_user.name);
        allocator.free(fetched_user.email);
    }
    try testing.expectEqual(user.id, fetched_user.id);
    try testing.expectEqualStrings("Alice", fetched_user.name);

    // 3. Update User
    try service.updateUser(user.id, "Alice Smith", null);
    const updated_user = try service.getUser(user.id);
    defer {
        allocator.free(updated_user.name);
        allocator.free(updated_user.email);
    }
    try testing.expectEqualStrings("Alice Smith", updated_user.name);
    try testing.expectEqualStrings("alice@example.com", updated_user.email);

    // 4. Delete User
    try service.deleteUser(user.id);
    if (service.getUser(user.id)) |_| {
        try testing.expect(false); // Should not exist
    } else |err| {
        try testing.expectEqual(error.NotFound, err);
    }
}

test "Integration Test: Task Flow" {
    const allocator = testing.allocator;

    var db = try DB.init(allocator, "test_tasks.db");
    defer {
        db.deinit();
        std.fs.cwd().deleteFile("test_tasks.db") catch {};
        std.fs.cwd().deleteFile("test_tasks.db-shm") catch {};
        std.fs.cwd().deleteFile("test_tasks.db-wal") catch {};
    }

    var repo = Repository.init(&db);
    var service = Service.init(&repo, allocator);

    const user = try service.createUser("Bob", "bob@example.com");

    // 1. Create Task
    const task = try service.createTask(user.id, "Buy Milk", "Whole milk");
    try testing.expect(task.id > 0);
    try testing.expectEqual(user.id, task.user_id);
    try testing.expectEqualStrings("Buy Milk", task.title);
    try testing.expect(task.done == false);

    // 2. Get Task
    const fetched_task = try service.getTask(task.id);
    defer {
        allocator.free(fetched_task.title);
        allocator.free(fetched_task.description);
    }
    try testing.expectEqual(task.id, fetched_task.id);

    // 3. Update Task
    try service.updateTask(task.id, "Buy Milk and Eggs", null, null);

    // 4. Mark Done
    try service.markTaskDone(task.id);

    const updated_task = try service.getTask(task.id);
    defer {
        allocator.free(updated_task.title);
        allocator.free(updated_task.description);
    }
    try testing.expectEqualStrings("Buy Milk and Eggs", updated_task.title);
    try testing.expect(updated_task.done == true);

    // 5. Get User Tasks
    const user_tasks = try service.getUserTasks(user.id);
    defer {
        for (user_tasks) |t| {
            allocator.free(t.title);
            allocator.free(t.description);
        }
        allocator.free(user_tasks);
    }
    try testing.expectEqual(1, user_tasks.len);
    try testing.expectEqual(task.id, user_tasks[0].id);

    // 6. Delete Task
    try service.deleteTask(task.id);
    if (service.getTask(task.id)) |_| {
        try testing.expect(false);
    } else |err| {
        try testing.expectEqual(error.NotFound, err);
    }
}
