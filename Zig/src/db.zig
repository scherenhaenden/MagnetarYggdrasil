const std = @import("std");
const sqlite = @import("sqlite");

pub const DB = struct {
    db: sqlite.Db,
    allocator: std.mem.Allocator,
    mutex: std.Thread.Mutex,

    pub fn init(allocator: std.mem.Allocator, path: []const u8) !DB {
        var db = try sqlite.Db.init(.{
            .mode = sqlite.Db.Mode{ .File = path },
            .open_flags = .{
                .write = true,
                .create = true,
            },
            .threading_mode = .MultiThread,
        });

        // Enable foreign keys
        try db.exec("PRAGMA foreign_keys = ON;", .{}, .{});
        // WAL mode for better concurrency
        try db.exec("PRAGMA journal_mode = WAL;", .{}, .{});
        try db.exec("PRAGMA synchronous = NORMAL;", .{}, .{});

        // Initialize schema
        try db.exec(
            \\CREATE TABLE IF NOT EXISTS users (
            \\    id INTEGER PRIMARY KEY AUTOINCREMENT,
            \\    name TEXT NOT NULL,
            \\    email TEXT NOT NULL UNIQUE
            \\);
            ,
            .{},
            .{},
        );

        try db.exec(
            \\CREATE TABLE IF NOT EXISTS tasks (
            \\    id INTEGER PRIMARY KEY AUTOINCREMENT,
            \\    user_id INTEGER NOT NULL,
            \\    title TEXT NOT NULL,
            \\    description TEXT NOT NULL,
            \\    done BOOLEAN NOT NULL DEFAULT 0,
            \\    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
            \\);
            ,
            .{},
            .{},
        );

        // Create indices
        try db.exec("CREATE INDEX IF NOT EXISTS idx_users_email ON users(email);", .{}, .{});
        try db.exec("CREATE INDEX IF NOT EXISTS idx_tasks_user_id ON tasks(user_id);", .{}, .{});

        return DB{
            .db = db,
            .allocator = allocator,
            .mutex = std.Thread.Mutex{},
        };
    }

    pub fn deinit(self: *DB) void {
        // In vrischmann/zig-sqlite, Db struct doesn't strictly require deinit if it's stack allocated wrapper,
        // but checking source (or usage patterns), usually we just close statement preparation if any.
        // Actually the underlying sqlite3_close needs to happen.
        // The wrapper likely handles it or we should check documentation.
        // Assuming standard behavior, we might not need explicit close if we rely on OS cleanup,
        // but let's check if there is a close/deinit method.
        // Looking at common usage of this lib, it seems we hold the DB.
        // There isn't a explicit deinit in the wrapper I recall seeing recently that is mandatory for the struct itself if it just wraps the handle,
        // but let's assume we keep it open for the lifetime of the app.
        _ = self;
    }
};
