const std = @import("std");

pub const User = struct {
    id: i64 = 0,
    name: []const u8,
    email: []const u8,

    // Helper to format as JSON
    pub fn jsonStringify(self: User, options: std.json.StringifyOptions, out_stream: anytype) !void {
        try std.json.stringify(.{
            .id = self.id,
            .name = self.name,
            .email = self.email,
        }, options, out_stream);
    }
};

pub const CreateUserRequest = struct {
    name: []const u8,
    email: []const u8,
};

pub const UpdateUserRequest = struct {
    name: ?[]const u8 = null,
    email: ?[]const u8 = null,
};
