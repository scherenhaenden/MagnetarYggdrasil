const std = @import("std");
const sqlite = @import("sqlite");
const User = @import("../models/user.zig").User;
const DB = @import("../storage/db.zig").DB;

pub const UserRepository = struct {
    db: *DB,

    pub fn init(db: *DB) UserRepository {
        return UserRepository{ .db = db };
    }

    pub fn create(self: *UserRepository, name: []const u8, email: []const u8, allocator: std.mem.Allocator) !User {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();

        const query = "INSERT INTO users (name, email) VALUES (?, ?) RETURNING id, name, email";
        var stmt = try self.db.db.prepare(query);
        defer stmt.deinit();

        const row = try stmt.oneAlloc(
            struct {
                id: i64,
                name: []const u8,
                email: []const u8,
            },
            allocator,
            .{ .name = name, .email = email },
        );

        if (row) |r| {
            return User{
                .id = r.id,
                .name = r.name,
                .email = r.email,
            };
        } else {
            return error.InsertFailed;
        }
    }

    pub fn getAll(self: *UserRepository, allocator: std.mem.Allocator) ![]User {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();

        const query = "SELECT id, name, email FROM users";
        var stmt = try self.db.db.prepare(query);
        defer stmt.deinit();

        var rows = std.ArrayList(User).init(allocator);
        errdefer rows.deinit();

        var iter = try stmt.iterator(
            struct {
                id: i64,
                name: []const u8,
                email: []const u8,
            },
            .{},
        );

        while (try iter.nextAlloc(allocator, .{})) |row| {
            try rows.append(User{
                .id = row.id,
                .name = row.name,
                .email = row.email,
            });
        }

        return rows.toOwnedSlice();
    }

    pub fn getById(self: *UserRepository, id: i64, allocator: std.mem.Allocator) !?User {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();

        const query = "SELECT id, name, email FROM users WHERE id = ?";
        var stmt = try self.db.db.prepare(query);
        defer stmt.deinit();

        const row = try stmt.oneAlloc(
            struct {
                id: i64,
                name: []const u8,
                email: []const u8,
            },
            allocator,
            .{ .id = id },
        );

        if (row) |r| {
            return User{
                .id = r.id,
                .name = r.name,
                .email = r.email,
            };
        } else {
            return null;
        }
    }

    pub fn update(self: *UserRepository, id: i64, name: ?[]const u8, email: ?[]const u8, allocator: std.mem.Allocator) !?User {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();

         // First check if user exists
        const check_query = "SELECT id FROM users WHERE id = ?";
        var check_stmt = try self.db.db.prepare(check_query);
        defer check_stmt.deinit();

        const exists = try check_stmt.one(struct { id: i64 }, .{ .id = id });
        if (exists == null) return null;

        if (name) |n| {
            const query = "UPDATE users SET name = ? WHERE id = ?";
            try self.db.db.exec(query, .{ .name = n, .id = id }, .{});
        }
        if (email) |e| {
            const query = "UPDATE users SET email = ? WHERE id = ?";
            try self.db.db.exec(query, .{ .email = e, .id = id }, .{});
        }

        // Re-fetch logic needs to be careful about mutex recursion if calling getById.
        // But getById also locks. So we should inline logic or use recursive mutex.
        // Zig Mutex is not recursive.
        // So we must duplicate getById logic here or extract it to a private method that expects lock to be held.
        // I will duplicate logic for safety and simplicity.

        const fetch_query = "SELECT id, name, email FROM users WHERE id = ?";
        var fetch_stmt = try self.db.db.prepare(fetch_query);
        defer fetch_stmt.deinit();

        const row = try fetch_stmt.oneAlloc(
            struct {
                id: i64,
                name: []const u8,
                email: []const u8,
            },
            allocator,
            .{ .id = id },
        );

        if (row) |r| {
            return User{
                .id = r.id,
                .name = r.name,
                .email = r.email,
            };
        } else {
            return null;
        }
    }

    pub fn delete(self: *UserRepository, id: i64) !bool {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();

        const query = "DELETE FROM users WHERE id = ?";
        try self.db.db.exec(query, .{ .id = id }, .{});

        return self.db.db.rowsModified() > 0;
    }
};
