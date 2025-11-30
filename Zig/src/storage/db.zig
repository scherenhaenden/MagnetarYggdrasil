const std = @import("std");
const sqlite = @import("sqlite");

pub const DB = struct {
    db: sqlite.Db,
    mutex: std.Thread.Mutex,

    pub fn init(allocator: std.mem.Allocator, path: []const u8) !DB {
        _ = allocator;

        const mode = if (std.mem.eql(u8, path, ":memory:"))
            sqlite.Db.Mode{ .Memory = {} }
        else
            sqlite.Db.Mode{ .File = path };

        var db = try sqlite.Db.init(.{
            .mode = mode,
            .open_flags = .{
                .write = true,
                .create = true,
            },
            .threading_mode = .Serialized, // Request thread-safe mode from SQLite itself
        });

        // We also add a mutex to be safe and explicitly lock if needed,
        // but .Serialized should handle it internally.
        // However, if we use `prepare` and `step` across different threads on the same connection, we might need care.
        // `zig-sqlite` `prepare` creates a statement. Statements are tied to connection.
        // If we use `exec` or `prepare` inside a lock, we are safe.
        // Let's use a mutex to guard the DB connection access.

        var self = DB{
            .db = db,
            .mutex = std.Thread.Mutex{},
        };

        try self.setupSchema();

        return self;
    }

    pub fn deinit(self: *DB) void {
        _ = self;
    }

    fn setupSchema(self: *DB) !void {
        self.mutex.lock();
        defer self.mutex.unlock();

        const user_table =
            \\CREATE TABLE IF NOT EXISTS users (
            \\  id INTEGER PRIMARY KEY AUTOINCREMENT,
            \\  name TEXT NOT NULL,
            \\  email TEXT NOT NULL UNIQUE
            \\);
        ;

        const task_table =
            \\CREATE TABLE IF NOT EXISTS tasks (
            \\  id INTEGER PRIMARY KEY AUTOINCREMENT,
            \\  user_id INTEGER NOT NULL,
            \\  title TEXT NOT NULL,
            \\  description TEXT NOT NULL,
            \\  done BOOLEAN NOT NULL DEFAULT 0,
            \\  FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE
            \\);
        ;

        // Enable foreign keys
        try self.db.exec("PRAGMA foreign_keys = ON;", .{}, .{});

        // WAL mode for better concurrency (only if file based, but harmless on memory?)
        try self.db.exec("PRAGMA journal_mode = WAL;", .{}, .{});

        try self.db.exec(user_table, .{}, .{});
        try self.db.exec(task_table, .{}, .{});
    }
};
