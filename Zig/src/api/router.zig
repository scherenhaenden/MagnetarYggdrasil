const std = @import("std");
const http = std.http;
const Handler = @import("handlers.zig").Handler;

pub const Router = struct {
    handler: *Handler,

    pub fn init(handler: *Handler) Router {
        return Router{ .handler = handler };
    }

    pub fn dispatch(self: *Router, req: *http.Server.Request) !void {
        const path = req.head.target;
        const method = req.head.method;

        if (std.mem.eql(u8, path, "/health")) {
            if (method == .GET) return self.handler.handleHealth(req);
        } else if (std.mem.eql(u8, path, "/users")) {
            if (method == .POST) return self.handler.handleCreateUser(req);
            if (method == .GET) return self.handler.handleGetAllUsers(req);
        } else if (std.mem.startsWith(u8, path, "/users/")) {
            const suffix = path["/users/".len..];
            if (suffix.len > 0) {
                // Check if it is /users/{id}/tasks
                if (std.mem.indexOf(u8, suffix, "/tasks")) |idx| {
                     const user_id = suffix[0..idx];
                     const rest = suffix[idx..];
                     if (std.mem.eql(u8, rest, "/tasks")) {
                         if (method == .POST) return self.handler.handleCreateTask(req, user_id);
                         if (method == .GET) return self.handler.handleGetTasksByUser(req, user_id);
                     }
                } else {
                    // It is /users/{id}
                    const user_id = suffix;
                    if (method == .GET) return self.handler.handleGetUserById(req, user_id);
                    if (method == .PUT) return self.handler.handleUpdateUser(req, user_id);
                    if (method == .DELETE) return self.handler.handleDeleteUser(req, user_id);
                }
            }
        } else if (std.mem.startsWith(u8, path, "/tasks/")) {
            const suffix = path["/tasks/".len..];
            if (suffix.len > 0) {
                // Check for /tasks/{tid}/done
                if (std.mem.endsWith(u8, suffix, "/done")) {
                    const task_id = suffix[0 .. suffix.len - "/done".len];
                    if (method == .PATCH) return self.handler.handleMarkTaskDone(req, task_id);
                } else {
                     const task_id = suffix;
                     if (method == .GET) return self.handler.handleGetTaskById(req, task_id);
                     if (method == .PUT) return self.handler.handleUpdateTask(req, task_id);
                     if (method == .DELETE) return self.handler.handleDeleteTask(req, task_id);
                }
            }
        }

        try req.respond("Not Found", .{ .status = .not_found });
    }
};
