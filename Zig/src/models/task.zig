const std = @import("std");

pub const Task = struct {
    id: i64 = 0,
    user_id: i64,
    title: []const u8,
    description: []const u8,
    done: bool = false,

    // Helper to format as JSON
    pub fn jsonStringify(self: Task, options: std.json.StringifyOptions, out_stream: anytype) !void {
        try std.json.stringify(.{
            .id = self.id,
            .user_id = self.user_id,
            .title = self.title,
            .description = self.description,
            .done = self.done,
        }, options, out_stream);
    }
};

pub const CreateTaskRequest = struct {
    title: []const u8,
    description: []const u8,
};

pub const UpdateTaskRequest = struct {
    title: ?[]const u8 = null,
    description: ?[]const u8 = null,
    done: ?bool = null,
};
