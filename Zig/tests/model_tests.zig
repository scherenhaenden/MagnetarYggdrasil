const std = @import("std");

test "User Model JSON" {
    const User = @import("../src/models/user.zig").User;

    const user = User{
        .id = 1,
        .name = "John Doe",
        .email = "john@example.com",
    };

    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();

    try std.json.stringify(user, .{}, buf.writer());

    const expected = "{\"id\":1,\"name\":\"John Doe\",\"email\":\"john@example.com\"}";
    try std.testing.expectEqualStrings(expected, buf.items);
}

test "Task Model JSON" {
    const Task = @import("../src/models/task.zig").Task;

    const task = Task{
        .id = 1,
        .user_id = 10,
        .title = "Do laundry",
        .description = "Wash clothes",
        .done = true,
    };

    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();

    try std.json.stringify(task, .{}, buf.writer());

    const expected = "{\"id\":1,\"user_id\":10,\"title\":\"Do laundry\",\"description\":\"Wash clothes\",\"done\":true}";
    try std.testing.expectEqualStrings(expected, buf.items);
}
