const std = @import("std");
const models = @import("models.zig");
const Repository = @import("repository.zig").Repository;

pub const Service = struct {
    repo: *Repository,
    allocator: std.mem.Allocator,

    pub fn init(repo: *Repository, allocator: std.mem.Allocator) Service {
        return Service{
            .repo = repo,
            .allocator = allocator,
        };
    }

    // Users
    pub fn createUser(self: *Service, name: []const u8, email: []const u8) !models.User {
        if (name.len == 0 or email.len == 0) return error.ValidationFailed;

        const user = models.User{
            .name = name,
            .email = email,
        };
        return self.repo.createUser(user);
    }

    pub fn getUser(self: *Service, id: i64) !models.User {
        if (try self.repo.getUser(id, self.allocator)) |user| {
            return user;
        }
        return error.NotFound;
    }

    pub fn getAllUsers(self: *Service) ![]models.User {
        return self.repo.getAllUsers(self.allocator);
    }

    pub fn updateUser(self: *Service, id: i64, name: ?[]const u8, email: ?[]const u8) !void {
        if (name == null and email == null) return; // Nothing to update
        try self.repo.updateUser(id, name, email);
    }

    pub fn deleteUser(self: *Service, id: i64) !void {
        try self.repo.deleteUser(id);
    }

    // Tasks
    pub fn createTask(self: *Service, user_id: i64, title: []const u8, description: []const u8) !models.Task {
        if (title.len == 0) return error.ValidationFailed;

        // Verify user exists
        if (try self.repo.getUser(user_id, self.allocator)) |user| {
             self.allocator.free(user.name);
             self.allocator.free(user.email);
        } else {
            return error.NotFound; // User not found
        }

        const task = models.Task{
            .user_id = user_id,
            .title = title,
            .description = description,
            .done = false,
        };
        return self.repo.createTask(task);
    }

    pub fn getTask(self: *Service, id: i64) !models.Task {
         if (try self.repo.getTask(id, self.allocator)) |task| {
            return task;
        }
        return error.NotFound;
    }

    pub fn getUserTasks(self: *Service, user_id: i64) ![]models.Task {
        // Verify user exists first? Maybe not strictly required by spec but good practice.
        // Spec says GET /users/{id}/tasks, implies 404 if user doesn't exist?
        // Or just empty list. Let's return tasks.
        return self.repo.getUserTasks(user_id, self.allocator);
    }

    pub fn updateTask(self: *Service, id: i64, title: ?[]const u8, description: ?[]const u8, done: ?bool) !void {
        try self.repo.updateTask(id, title, description, done);
    }

    pub fn markTaskDone(self: *Service, id: i64) !void {
        try self.repo.markTaskDone(id);
    }

    pub fn deleteTask(self: *Service, id: i64) !void {
        try self.repo.deleteTask(id);
    }
};
