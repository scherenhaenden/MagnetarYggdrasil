const std = @import("std");
const sqlite = @import("sqlite");
const Task = @import("../models/task.zig").Task;
const DB = @import("../storage/db.zig").DB;

pub const TaskRepository = struct {
    db: *DB,

    pub fn init(db: *DB) TaskRepository {
        return TaskRepository{ .db = db };
    }

    pub fn create(self: *TaskRepository, user_id: i64, title: []const u8, description: []const u8, allocator: std.mem.Allocator) !Task {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();

        const query = "INSERT INTO tasks (user_id, title, description, done) VALUES (?, ?, ?, 0) RETURNING id, user_id, title, description, done";
        var stmt = try self.db.db.prepare(query);
        defer stmt.deinit();

        const row = try stmt.oneAlloc(
            struct {
                id: i64,
                user_id: i64,
                title: []const u8,
                description: []const u8,
                done: bool,
            },
            allocator,
            .{ .user_id = user_id, .title = title, .description = description },
        );

        if (row) |r| {
            return Task{
                .id = r.id,
                .user_id = r.user_id,
                .title = r.title,
                .description = r.description,
                .done = r.done,
            };
        } else {
            return error.InsertFailed;
        }
    }

    pub fn getAllByUserId(self: *TaskRepository, user_id: i64, allocator: std.mem.Allocator) ![]Task {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();

        const query = "SELECT id, user_id, title, description, done FROM tasks WHERE user_id = ?";
        var stmt = try self.db.db.prepare(query);
        defer stmt.deinit();

        var rows = std.ArrayList(Task).init(allocator);
        errdefer rows.deinit();

        var iter = try stmt.iterator(
            struct {
                id: i64,
                user_id: i64,
                title: []const u8,
                description: []const u8,
                done: bool,
            },
            .{ .user_id = user_id },
        );

        while (try iter.nextAlloc(allocator, .{})) |row| {
            try rows.append(Task{
                .id = row.id,
                .user_id = row.user_id,
                .title = row.title,
                .description = row.description,
                .done = row.done,
            });
        }

        return rows.toOwnedSlice();
    }

    pub fn getById(self: *TaskRepository, id: i64, allocator: std.mem.Allocator) !?Task {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();

        const query = "SELECT id, user_id, title, description, done FROM tasks WHERE id = ?";
        var stmt = try self.db.db.prepare(query);
        defer stmt.deinit();

        const row = try stmt.oneAlloc(
            struct {
                id: i64,
                user_id: i64,
                title: []const u8,
                description: []const u8,
                done: bool,
            },
            allocator,
            .{ .id = id },
        );

        if (row) |r| {
            return Task{
                .id = r.id,
                .user_id = r.user_id,
                .title = r.title,
                .description = r.description,
                .done = r.done,
            };
        } else {
            return null;
        }
    }

    pub fn update(self: *TaskRepository, id: i64, title: ?[]const u8, description: ?[]const u8, done: ?bool, allocator: std.mem.Allocator) !?Task {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();

        // First check if task exists
        const check_query = "SELECT id FROM tasks WHERE id = ?";
        var check_stmt = try self.db.db.prepare(check_query);
        defer check_stmt.deinit();

        const exists = try check_stmt.one(struct { id: i64 }, .{ .id = id });
        if (exists == null) return null;

        if (title) |t| {
            const query = "UPDATE tasks SET title = ? WHERE id = ?";
            try self.db.db.exec(query, .{ .title = t, .id = id }, .{});
        }
        if (description) |d| {
            const query = "UPDATE tasks SET description = ? WHERE id = ?";
            try self.db.db.exec(query, .{ .description = d, .id = id }, .{});
        }
        if (done) |d| {
            const query = "UPDATE tasks SET done = ? WHERE id = ?";
            try self.db.db.exec(query, .{ .done = d, .id = id }, .{});
        }

        // Fetch
        const fetch_query = "SELECT id, user_id, title, description, done FROM tasks WHERE id = ?";
        var fetch_stmt = try self.db.db.prepare(fetch_query);
        defer fetch_stmt.deinit();

        const row = try fetch_stmt.oneAlloc(
            struct {
                id: i64,
                user_id: i64,
                title: []const u8,
                description: []const u8,
                done: bool,
            },
            allocator,
            .{ .id = id },
        );

        if (row) |r| {
            return Task{
                .id = r.id,
                .user_id = r.user_id,
                .title = r.title,
                .description = r.description,
                .done = r.done,
            };
        } else {
            return null;
        }
    }

    pub fn markDone(self: *TaskRepository, id: i64) !bool {
         self.db.mutex.lock();
         defer self.db.mutex.unlock();

         const query = "UPDATE tasks SET done = 1 WHERE id = ?";
         try self.db.db.exec(query, .{ .id = id }, .{});
         return self.db.db.rowsModified() > 0;
    }

    pub fn delete(self: *TaskRepository, id: i64) !bool {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();

        const query = "DELETE FROM tasks WHERE id = ?";
        try self.db.db.exec(query, .{ .id = id }, .{});

        return self.db.db.rowsModified() > 0;
    }
};
