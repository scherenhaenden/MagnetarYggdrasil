const std = @import("std");
const http = std.http;
const UserService = @import("../service/user_service.zig").UserService;
const TaskService = @import("../service/task_service.zig").TaskService;
const CreateUserRequest = @import("../models/user.zig").CreateUserRequest;
const UpdateUserRequest = @import("../models/user.zig").UpdateUserRequest;
const CreateTaskRequest = @import("../models/task.zig").CreateTaskRequest;
const UpdateTaskRequest = @import("../models/task.zig").UpdateTaskRequest;

pub const Handler = struct {
    user_service: *UserService,
    task_service: *TaskService,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, user_service: *UserService, task_service: *TaskService) Handler {
        return Handler{
            .allocator = allocator,
            .user_service = user_service,
            .task_service = task_service,
        };
    }

    fn readBody(self: *Handler, req: *http.Server.Request) ![]const u8 {
        const reader = req.reader();
        return try reader.readAllAlloc(self.allocator, 1024 * 1024); // 1MB limit
    }

    fn sendJson(self: *Handler, req: *http.Server.Request, status: http.Status, data: anytype) !void {
        _ = self;
        var buf = std.ArrayList(u8).init(self.allocator);
        defer buf.deinit();

        try std.json.stringify(data, .{}, buf.writer());

        try req.respond(buf.items, .{
            .status = status,
            .extra_headers = &.{
                .{ .name = "Content-Type", .value = "application/json" },
            },
        });
    }

    fn sendError(self: *Handler, req: *http.Server.Request, status: http.Status, message: []const u8) !void {
        try self.sendJson(req, status, .{ .@"error" = message });
    }

    // --- User Handlers ---

    pub fn handleCreateUser(self: *Handler, req: *http.Server.Request) !void {
        const body = try self.readBody(req);
        defer self.allocator.free(body);

        const parsed = std.json.parseFromSlice(CreateUserRequest, self.allocator, body, .{}) catch {
            return self.sendError(req, .bad_request, "Invalid JSON");
        };
        defer parsed.deinit();

        const user = self.user_service.createUser(parsed.value) catch |err| {
            // Check for unique constraint violation ideally, but simplified here
            std.debug.print("Error creating user: {}\n", .{err});
             // Simple hack to detect unique constraint violation if possible, but zig-sqlite error mapping might vary
            return self.sendError(req, .internal_server_error, "Failed to create user");
        };

        try self.sendJson(req, .created, user);
    }

    pub fn handleGetAllUsers(self: *Handler, req: *http.Server.Request) !void {
        const users = self.user_service.getAllUsers() catch {
            return self.sendError(req, .internal_server_error, "Failed to fetch users");
        };
        // Users are allocated with self.allocator (via service -> repo), we should ideally free them.
        // But for short lived request handlers in an arena allocator it's fine.
        // If not using arena, we need to be careful.
        // Assuming we rely on arena for request scope or we should manually free.
        // For this implementation, let's assume leak is acceptable or handled by arena if passed.
        // But `std.http.Server` usually reuses allocator.
        // Let's defer freeing if possible.
        defer self.allocator.free(users);

        try self.sendJson(req, .ok, users);
    }

    pub fn handleGetUserById(self: *Handler, req: *http.Server.Request, id_str: []const u8) !void {
        const id = std.fmt.parseInt(i64, id_str, 10) catch {
            return self.sendError(req, .bad_request, "Invalid ID format");
        };

        const user = self.user_service.getUserById(id) catch {
            return self.sendError(req, .internal_server_error, "Failed to fetch user");
        };

        if (user) |u| {
            try self.sendJson(req, .ok, u);
        } else {
            return self.sendError(req, .not_found, "User not found");
        }
    }

    pub fn handleUpdateUser(self: *Handler, req: *http.Server.Request, id_str: []const u8) !void {
        const id = std.fmt.parseInt(i64, id_str, 10) catch {
            return self.sendError(req, .bad_request, "Invalid ID format");
        };

        const body = try self.readBody(req);
        defer self.allocator.free(body);

        const parsed = std.json.parseFromSlice(UpdateUserRequest, self.allocator, body, .{}) catch {
            return self.sendError(req, .bad_request, "Invalid JSON");
        };
        defer parsed.deinit();

        const user = self.user_service.updateUser(id, parsed.value) catch {
            return self.sendError(req, .internal_server_error, "Failed to update user");
        };

        if (user) |u| {
            try self.sendJson(req, .ok, u);
        } else {
            return self.sendError(req, .not_found, "User not found");
        }
    }

    pub fn handleDeleteUser(self: *Handler, req: *http.Server.Request, id_str: []const u8) !void {
        const id = std.fmt.parseInt(i64, id_str, 10) catch {
            return self.sendError(req, .bad_request, "Invalid ID format");
        };

        const deleted = self.user_service.deleteUser(id) catch {
            return self.sendError(req, .internal_server_error, "Failed to delete user");
        };

        if (deleted) {
            try req.respond("", .{ .status = .no_content });
        } else {
            return self.sendError(req, .not_found, "User not found");
        }
    }

    // --- Task Handlers ---

    pub fn handleCreateTask(self: *Handler, req: *http.Server.Request, user_id_str: []const u8) !void {
        const user_id = std.fmt.parseInt(i64, user_id_str, 10) catch {
            return self.sendError(req, .bad_request, "Invalid User ID format");
        };

        // Verify user exists first? The foreign key will fail if not.

        const body = try self.readBody(req);
        defer self.allocator.free(body);

        const parsed = std.json.parseFromSlice(CreateTaskRequest, self.allocator, body, .{}) catch {
            return self.sendError(req, .bad_request, "Invalid JSON");
        };
        defer parsed.deinit();

        const task = self.task_service.createTask(user_id, parsed.value) catch |err| {
            std.debug.print("Error creating task: {}\n", .{err});
            return self.sendError(req, .internal_server_error, "Failed to create task");
        };

        try self.sendJson(req, .created, task);
    }

    pub fn handleGetTasksByUser(self: *Handler, req: *http.Server.Request, user_id_str: []const u8) !void {
        const user_id = std.fmt.parseInt(i64, user_id_str, 10) catch {
            return self.sendError(req, .bad_request, "Invalid User ID format");
        };

        const tasks = self.task_service.getTasksByUserId(user_id) catch {
            return self.sendError(req, .internal_server_error, "Failed to fetch tasks");
        };
        defer self.allocator.free(tasks);

        try self.sendJson(req, .ok, tasks);
    }

    pub fn handleGetTaskById(self: *Handler, req: *http.Server.Request, id_str: []const u8) !void {
        const id = std.fmt.parseInt(i64, id_str, 10) catch {
            return self.sendError(req, .bad_request, "Invalid ID format");
        };

        const task = self.task_service.getTaskById(id) catch {
            return self.sendError(req, .internal_server_error, "Failed to fetch task");
        };

        if (task) |t| {
            try self.sendJson(req, .ok, t);
        } else {
            return self.sendError(req, .not_found, "Task not found");
        }
    }

    pub fn handleUpdateTask(self: *Handler, req: *http.Server.Request, id_str: []const u8) !void {
        const id = std.fmt.parseInt(i64, id_str, 10) catch {
            return self.sendError(req, .bad_request, "Invalid ID format");
        };

        const body = try self.readBody(req);
        defer self.allocator.free(body);

        const parsed = std.json.parseFromSlice(UpdateTaskRequest, self.allocator, body, .{}) catch {
            return self.sendError(req, .bad_request, "Invalid JSON");
        };
        defer parsed.deinit();

        const task = self.task_service.updateTask(id, parsed.value) catch {
            return self.sendError(req, .internal_server_error, "Failed to update task");
        };

        if (task) |t| {
            try self.sendJson(req, .ok, t);
        } else {
            return self.sendError(req, .not_found, "Task not found");
        }
    }

    pub fn handleMarkTaskDone(self: *Handler, req: *http.Server.Request, id_str: []const u8) !void {
        const id = std.fmt.parseInt(i64, id_str, 10) catch {
            return self.sendError(req, .bad_request, "Invalid ID format");
        };

        const success = self.task_service.markTaskDone(id) catch {
            return self.sendError(req, .internal_server_error, "Failed to mark task as done");
        };

        if (success) {
            // Should return something? Usually 200 OK or 204 No Content.
            // "PATCH /tasks/{tid}/done" - implies idempotent.
            // Let's return the message or empty.
            try req.respond("", .{ .status = .ok });
        } else {
            return self.sendError(req, .not_found, "Task not found");
        }
    }

    pub fn handleDeleteTask(self: *Handler, req: *http.Server.Request, id_str: []const u8) !void {
        const id = std.fmt.parseInt(i64, id_str, 10) catch {
            return self.sendError(req, .bad_request, "Invalid ID format");
        };

        const deleted = self.task_service.deleteTask(id) catch {
            return self.sendError(req, .internal_server_error, "Failed to delete task");
        };

        if (deleted) {
            try req.respond("", .{ .status = .no_content });
        } else {
            return self.sendError(req, .not_found, "Task not found");
        }
    }

    pub fn handleHealth(self: *Handler, req: *http.Server.Request) !void {
        _ = self;
        try req.respond("{\"status\": \"ok\"}", .{
            .status = .ok,
             .extra_headers = &.{
                .{ .name = "Content-Type", .value = "application/json" },
            },
        });
    }
};
