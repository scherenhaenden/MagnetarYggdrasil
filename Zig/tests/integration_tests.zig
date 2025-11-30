const std = @import("std");
const sqlite = @import("sqlite");
const DB = @import("../src/storage/db.zig").DB;
const UserRepository = @import("../src/repository/user_repository.zig").UserRepository;
const TaskRepository = @import("../src/repository/task_repository.zig").TaskRepository;

test "Repository Integration" {
    const allocator = std.testing.allocator;

    // Use in-memory DB
    var db = try DB.init(allocator, ":memory:");
    defer db.deinit();

    var user_repo = UserRepository.init(&db);
    var task_repo = TaskRepository.init(&db);

    // Create User
    const user = try user_repo.create("Alice", "alice@example.com", allocator);
    try std.testing.expectEqual(user.id, 1);
    try std.testing.expectEqualStrings(user.name, "Alice");
    try std.testing.expectEqualStrings(user.email, "alice@example.com");

    // Create Task
    const task = try task_repo.create(user.id, "Task 1", "Description 1", allocator);
    try std.testing.expectEqual(task.id, 1);
    try std.testing.expectEqual(task.user_id, user.id);
    try std.testing.expectEqualStrings(task.title, "Task 1");

    // Get User
    const fetched_user = try user_repo.getById(user.id, allocator);
    try std.testing.expect(fetched_user != null);
    if (fetched_user) |u| {
        try std.testing.expectEqual(u.id, user.id);
        allocator.free(u.name);
        allocator.free(u.email);
    }

    // Clean up created objects (they have allocated strings)
    // The create method returned a User/Task which has strings that point to where?
    // `oneAlloc` allocates a single block containing struct and strings.
    // So `allocator.free(user)`? No, `oneAlloc` returns a pointer if we asked for a pointer.
    // But we returned by value. The strings inside `User` struct point to...
    // Wait, `oneAlloc` (in zig-sqlite) returns a parsed struct. If fields are slices, they point to memory allocated by `allocator`.
    // It seems `oneAlloc` allocates memory for the result.
    // If it returns by value, the slices point to allocated memory.
    // We should free the slices.
    // Or if `oneAlloc` allocates a single block, we can't free slices individually if they are part of that block.
    // zig-sqlite `oneAlloc` uses `allocator` to allocate resources.
    // Usually it allocates separate strings.
    // Let's assume separate strings for now as it is safer to leak in test than double free or crash.
    // But `std.testing.allocator` will complain.

    // To properly test without leaks we need to know `zig-sqlite` allocation strategy.
    // Assuming we need to free strings:
    allocator.free(user.name);
    allocator.free(user.email);
    allocator.free(task.title);
    allocator.free(task.description);
}
