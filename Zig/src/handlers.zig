const std = @import("std");
const http = std.http;
const Service = @import("service.zig").Service;
const models = @import("models.zig");

// Helper to send JSON response
fn sendJson(req: *http.Server.Request, status: http.Status, data: anytype) !void {
    const json = try std.json.stringifyAlloc(req.server.allocator, data, .{});
    defer req.server.allocator.free(json);

    try req.respond(json, .{
        .status = status,
        .extra_headers = &.{
            .{ .name = "Content-Type", .value = "application/json" },
        },
    });
}

// Helper to send Error response
fn sendError(req: *http.Server.Request, status: http.Status, message: []const u8) !void {
    try sendJson(req, status, .{ .@"error" = message });
}

pub const Handler = struct {
    service: *Service,
    allocator: std.mem.Allocator,

    pub fn init(service: *Service, allocator: std.mem.Allocator) Handler {
        return Handler{ .service = service, .allocator = allocator };
    }

    pub fn handle(self: *Handler, req: *http.Server.Request, res: *http.Server.Response) !void {
        _ = res; // Response is handled via req.respond in std.http new API (or similar depending on version)
        // Wait, std.http API changes frequently.
        // In recent versions (0.12/0.13), we typically use `req.respond`.

        const path = req.head.target;
        const method = req.head.method;

        // Routing logic
        if (std.mem.eql(u8, path, "/health")) {
             try sendJson(req, .ok, .{ .status = "ok" });
             return;
        }

        if (std.mem.startsWith(u8, path, "/users")) {
            return self.handleUsers(req, method, path[6..]);
        }

        if (std.mem.startsWith(u8, path, "/tasks")) {
            return self.handleTasks(req, method, path[6..]);
        }

        try sendError(req, .not_found, "Not Found");
    }

    fn handleUsers(self: *Handler, req: *http.Server.Request, method: http.Method, path: []const u8) !void {
        // /users
        if (path.len == 0 or std.mem.eql(u8, path, "/")) {
            switch (method) {
                .GET => {
                    const users = try self.service.getAllUsers();
                    defer {
                        for (users) |u| {
                            self.allocator.free(u.name);
                            self.allocator.free(u.email);
                        }
                        self.allocator.free(users);
                    }
                    try sendJson(req, .ok, users);
                },
                .POST => {
                    const Body = struct { name: []const u8, email: []const u8 };

                    const reader = req.reader();
                    const body_bytes = try reader.readAllAlloc(self.allocator, 1024 * 1024);
                    defer self.allocator.free(body_bytes);

                    const parsed = try std.json.parseFromSlice(Body, self.allocator, body_bytes, .{});
                    defer parsed.deinit();

                    const user = try self.service.createUser(parsed.value.name, parsed.value.email);
                    try sendJson(req, .created, user);
                },
                else => try sendError(req, .method_not_allowed, "Method Not Allowed"),
            }
            return;
        }

        // /users/{id} or /users/{id}/tasks
        if (path[0] == '/') {
            const subpath = path[1..];
            const slash_pos = std.mem.indexOf(u8, subpath, "/");

            var id_str: []const u8 = undefined;
            var remaining: []const u8 = "";

            if (slash_pos) |pos| {
                id_str = subpath[0..pos];
                remaining = subpath[pos..];
            } else {
                id_str = subpath;
            }

            const id = std.fmt.parseInt(i64, id_str, 10) catch {
                try sendError(req, .bad_request, "Invalid ID");
                return;
            };

            if (remaining.len == 0) {
                 // /users/{id}
                 switch (method) {
                     .GET => {
                         const user = self.service.getUser(id) catch |err| {
                             if (err == error.NotFound) {
                                 try sendError(req, .not_found, "User not found");
                                 return;
                             }
                             return err;
                         };
                         defer {
                             self.allocator.free(user.name);
                             self.allocator.free(user.email);
                         }
                         try sendJson(req, .ok, user);
                     },
                     .PUT => {
                         const Body = struct { name: ?[]const u8 = null, email: ?[]const u8 = null };
                         const reader = req.reader();
                         const body_bytes = try reader.readAllAlloc(self.allocator, 1024 * 1024);
                         defer self.allocator.free(body_bytes);

                         const parsed = try std.json.parseFromSlice(Body, self.allocator, body_bytes, .{});
                         defer parsed.deinit();

                         self.service.updateUser(id, parsed.value.name, parsed.value.email) catch |err| {
                             if (err == error.NotFound) {
                                 try sendError(req, .not_found, "User not found");
                                 return;
                             }
                             return err;
                         };
                         try sendJson(req, .ok, .{ .status = "updated" });
                     },
                     .DELETE => {
                         self.service.deleteUser(id) catch |err| {
                             if (err == error.NotFound) {
                                 try sendError(req, .not_found, "User not found");
                                 return;
                             }
                             return err;
                         };
                         try sendJson(req, .no_content, .{}); // Or 200 OK
                     },
                     else => try sendError(req, .method_not_allowed, "Method Not Allowed"),
                 }
            } else if (std.mem.eql(u8, remaining, "/tasks")) {
                 // /users/{id}/tasks
                 switch (method) {
                     .GET => {
                         const tasks = self.service.getUserTasks(id) catch |err| {
                              // If user not found, service might return empty or error.
                              // Assuming empty list if user exists but has no tasks, or error if user invalid.
                              return err;
                         };
                         defer {
                             for (tasks) |t| {
                                 self.allocator.free(t.title);
                                 self.allocator.free(t.description);
                             }
                             self.allocator.free(tasks);
                         }
                         try sendJson(req, .ok, tasks);
                     },
                     .POST => {
                         const Body = struct { title: []const u8, description: []const u8 };
                         const reader = req.reader();
                         const body_bytes = try reader.readAllAlloc(self.allocator, 1024 * 1024);
                         defer self.allocator.free(body_bytes);

                         const parsed = try std.json.parseFromSlice(Body, self.allocator, body_bytes, .{});
                         defer parsed.deinit();

                         const task = self.service.createTask(id, parsed.value.title, parsed.value.description) catch |err| {
                             if (err == error.NotFound) {
                                 try sendError(req, .not_found, "User not found");
                                 return;
                             }
                             return err;
                         };
                         try sendJson(req, .created, task);
                     },
                     else => try sendError(req, .method_not_allowed, "Method Not Allowed"),
                 }
            } else {
                 try sendError(req, .not_found, "Not Found");
            }
            return;
        }

        try sendError(req, .not_found, "Not Found");
    }

    fn handleTasks(self: *Handler, req: *http.Server.Request, method: http.Method, path: []const u8) !void {
        // /tasks/{id}
         if (path.len > 1 and path[0] == '/') {
            const subpath = path[1..];
             // check if there are more slashes
            var id_str: []const u8 = subpath;
            var action: []const u8 = "";

            if (std.mem.indexOf(u8, subpath, "/")) |pos| {
                id_str = subpath[0..pos];
                action = subpath[pos..];
            }

            const id = std.fmt.parseInt(i64, id_str, 10) catch {
                try sendError(req, .bad_request, "Invalid ID");
                return;
            };

            if (action.len == 0) {
                 // /tasks/{id}
                 switch (method) {
                     .GET => {
                         const task = self.service.getTask(id) catch |err| {
                             if (err == error.NotFound) {
                                 try sendError(req, .not_found, "Task not found");
                                 return;
                             }
                             return err;
                         };
                         defer {
                             self.allocator.free(task.title);
                             self.allocator.free(task.description);
                         }
                         try sendJson(req, .ok, task);
                     },
                     .PUT => {
                         const Body = struct { title: ?[]const u8 = null, description: ?[]const u8 = null, done: ?bool = null };
                         const reader = req.reader();
                         const body_bytes = try reader.readAllAlloc(self.allocator, 1024 * 1024);
                         defer self.allocator.free(body_bytes);

                         const parsed = try std.json.parseFromSlice(Body, self.allocator, body_bytes, .{});
                         defer parsed.deinit();

                         self.service.updateTask(id, parsed.value.title, parsed.value.description, parsed.value.done) catch |err| {
                             if (err == error.NotFound) {
                                 try sendError(req, .not_found, "Task not found");
                                 return;
                             }
                             return err;
                         };
                         try sendJson(req, .ok, .{ .status = "updated" });
                     },
                     .DELETE => {
                         self.service.deleteTask(id) catch |err| {
                             if (err == error.NotFound) {
                                 try sendError(req, .not_found, "Task not found");
                                 return;
                             }
                             return err;
                         };
                         try sendJson(req, .no_content, .{});
                     },
                     else => try sendError(req, .method_not_allowed, "Method Not Allowed"),
                 }
            } else if (std.mem.eql(u8, action, "/done")) {
                // /tasks/{id}/done
                if (method == .PATCH) {
                    self.service.markTaskDone(id) catch |err| {
                         if (err == error.NotFound) {
                             try sendError(req, .not_found, "Task not found");
                             return;
                         }
                         return err;
                    };
                    try sendJson(req, .ok, .{ .status = "marked done" });
                } else {
                    try sendError(req, .method_not_allowed, "Method Not Allowed");
                }
            } else {
                try sendError(req, .not_found, "Not Found");
            }
            return;
        }
        try sendError(req, .not_found, "Not Found");
    }
};
