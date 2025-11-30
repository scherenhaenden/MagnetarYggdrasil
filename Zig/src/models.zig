const std = @import("std");

pub const User = struct {
    id: i64 = 0,
    name: []const u8,
    email: []const u8,
};

pub const Task = struct {
    id: i64 = 0,
    user_id: i64,
    title: []const u8,
    description: []const u8,
    done: bool = false,
};
