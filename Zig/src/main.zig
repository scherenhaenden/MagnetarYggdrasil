const std = @import("std");
const DB = @import("storage/db.zig").DB;
const Server = @import("api/server.zig").Server;

// Tests
test {
    // Import all files to run tests
    _ = @import("models/user.zig");
    _ = @import("models/task.zig");
    _ = @import("storage/db.zig");
    _ = @import("repository/user_repository.zig");
    _ = @import("repository/task_repository.zig");
    _ = @import("service/user_service.zig");
    _ = @import("service/task_service.zig");
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Initialize Database
    var db = try DB.init(allocator, "database.db");
    defer db.deinit();

    // Initialize and Start Server
    var server = Server.init(allocator, 8080, &db);
    try server.start();
}
