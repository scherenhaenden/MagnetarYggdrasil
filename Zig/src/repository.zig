const std = @import("std");
const sqlite = @import("sqlite");
const models = @import("models.zig");
const DB = @import("db.zig").DB;

pub const Repository = struct {
    db: *DB,

    pub fn init(db: *DB) Repository {
        return Repository{ .db = db };
    }

    // Users
    pub fn createUser(self: *Repository, user: models.User) !models.User {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();
        const query = "INSERT INTO users (name, email) VALUES (?, ?) RETURNING id";
        var stmt = try self.db.db.prepare(query);
        defer stmt.deinit();

        const id = try stmt.one(i64, .{ user.name, user.email });
        if (id) |new_id| {
            var new_user = user;
            new_user.id = new_id;
            return new_user;
        }
        return error.InsertFailed;
    }

    pub fn getUser(self: *Repository, id: i64, allocator: std.mem.Allocator) !?models.User {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();
        const query = "SELECT id, name, email FROM users WHERE id = ?";
        var stmt = try self.db.db.prepare(query);
        defer stmt.deinit();

        const row = try stmt.one(
            struct { id: i64, name: []const u8, email: []const u8 },
            .{id},
        );

        if (row) |r| {
            return models.User{
                .id = r.id,
                .name = try allocator.dupe(u8, r.name),
                .email = try allocator.dupe(u8, r.email),
            };
        }
        return null;
    }

    pub fn getAllUsers(self: *Repository, allocator: std.mem.Allocator) ![]models.User {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();
        const query = "SELECT id, name, email FROM users";
        var stmt = try self.db.db.prepare(query);
        defer stmt.deinit();

        var users = std.ArrayList(models.User).init(allocator);
        errdefer {
            for (users.items) |u| {
                allocator.free(u.name);
                allocator.free(u.email);
            }
            users.deinit();
        }

        while (try stmt.step()) |row| {
            const id = row.get(i64, 0);
            const name = row.get([]const u8, 1);
            const email = row.get([]const u8, 2);

            try users.append(models.User{
                .id = id,
                .name = try allocator.dupe(u8, name),
                .email = try allocator.dupe(u8, email),
            });
        }

        return users.toOwnedSlice();
    }

    pub fn updateUser(self: *Repository, id: i64, name: ?[]const u8, email: ?[]const u8) !void {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();
        // Build dynamic query or use COALESCE
        const query = "UPDATE users SET name = COALESCE(?, name), email = COALESCE(?, email) WHERE id = ?";
        var stmt = try self.db.db.prepare(query);
        defer stmt.deinit();

        try stmt.exec(.{ name, email, id });

        if (self.db.db.rowsModified() == 0) {
            return error.NotFound;
        }
    }

    pub fn deleteUser(self: *Repository, id: i64) !void {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();
        const query = "DELETE FROM users WHERE id = ?";
        var stmt = try self.db.db.prepare(query);
        defer stmt.deinit();

        try stmt.exec(.{id});

        if (self.db.db.rowsModified() == 0) {
            return error.NotFound;
        }
    }

    // Tasks
    pub fn createTask(self: *Repository, task: models.Task) !models.Task {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();
        // First check if user exists to enforce logic (though FK handles it, we might want custom error)
        // FK constraint will fail if user doesn't exist.

        const query = "INSERT INTO tasks (user_id, title, description, done) VALUES (?, ?, ?, ?) RETURNING id";
        var stmt = try self.db.db.prepare(query);
        defer stmt.deinit();

        const id = try stmt.one(i64, .{ task.user_id, task.title, task.description, task.done });
        if (id) |new_id| {
            var new_task = task;
            new_task.id = new_id;
            return new_task;
        }
        return error.InsertFailed;
    }

    pub fn getTask(self: *Repository, id: i64, allocator: std.mem.Allocator) !?models.Task {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();
        const query = "SELECT id, user_id, title, description, done FROM tasks WHERE id = ?";
        var stmt = try self.db.db.prepare(query);
        defer stmt.deinit();

        const row = try stmt.one(
            struct { id: i64, user_id: i64, title: []const u8, description: []const u8, done: bool },
            .{id},
        );

        if (row) |r| {
            return models.Task{
                .id = r.id,
                .user_id = r.user_id,
                .title = try allocator.dupe(u8, r.title),
                .description = try allocator.dupe(u8, r.description),
                .done = r.done,
            };
        }
        return null;
    }

    pub fn getUserTasks(self: *Repository, user_id: i64, allocator: std.mem.Allocator) ![]models.Task {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();
        const query = "SELECT id, user_id, title, description, done FROM tasks WHERE user_id = ?";
        var stmt = try self.db.db.prepare(query);
        defer stmt.deinit();

        var tasks = std.ArrayList(models.Task).init(allocator);
        errdefer {
            for (tasks.items) |t| {
                allocator.free(t.title);
                allocator.free(t.description);
            }
            tasks.deinit();
        }

        while (try stmt.step()) |row| {
             const id = row.get(i64, 0);
             const uid = row.get(i64, 1);
             const title = row.get([]const u8, 2);
             const description = row.get([]const u8, 3);
             const done = row.get(bool, 4);

             try tasks.append(models.Task{
                 .id = id,
                 .user_id = uid,
                 .title = try allocator.dupe(u8, title),
                 .description = try allocator.dupe(u8, description),
                 .done = done,
             });
        }
        return tasks.toOwnedSlice();
    }

    pub fn updateTask(self: *Repository, id: i64, title: ?[]const u8, description: ?[]const u8, done: ?bool) !void {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();
        const query = "UPDATE tasks SET title = COALESCE(?, title), description = COALESCE(?, description), done = COALESCE(?, done) WHERE id = ?";
        var stmt = try self.db.db.prepare(query);
        defer stmt.deinit();

        try stmt.exec(.{ title, description, done, id });

        if (self.db.db.rowsModified() == 0) {
            return error.NotFound;
        }
    }

    pub fn markTaskDone(self: *Repository, id: i64) !void {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();
        const query = "UPDATE tasks SET done = 1 WHERE id = ?";
        var stmt = try self.db.db.prepare(query);
        defer stmt.deinit();

        try stmt.exec(.{id});

        if (self.db.db.rowsModified() == 0) {
            return error.NotFound;
        }
    }

    pub fn deleteTask(self: *Repository, id: i64) !void {
        self.db.mutex.lock();
        defer self.db.mutex.unlock();
        const query = "DELETE FROM tasks WHERE id = ?";
        var stmt = try self.db.db.prepare(query);
        defer stmt.deinit();

        try stmt.exec(.{id});

        if (self.db.db.rowsModified() == 0) {
            return error.NotFound;
        }
    }
};
