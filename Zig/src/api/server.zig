const std = @import("std");
const http = std.http;
const Router = @import("router.zig").Router;
const DB = @import("../storage/db.zig").DB;
const UserRepository = @import("../repository/user_repository.zig").UserRepository;
const TaskRepository = @import("../repository/task_repository.zig").TaskRepository;
const UserService = @import("../service/user_service.zig").UserService;
const TaskService = @import("../service/task_service.zig").TaskService;
const Handler = @import("handlers.zig").Handler;

pub const Server = struct {
    port: u16,
    allocator: std.mem.Allocator,
    db: *DB,

    pub fn init(allocator: std.mem.Allocator, port: u16, db: *DB) Server {
        return Server{
            .allocator = allocator,
            .port = port,
            .db = db,
        };
    }

    pub fn start(self: *Server) !void {
        const address = try std.net.Address.parseIp("0.0.0.0", self.port);
        var server = try address.listen(.{
             .reuse_address = true,
        });
        defer server.deinit();

        std.debug.print("Listening on http://0.0.0.0:{}\n", .{self.port});

        while (true) {
            const connection = try server.accept();
            const thread = try std.Thread.spawn(.{}, handleConnection, .{ self.allocator, connection, self.db });
            thread.detach();
        }
    }

    fn handleConnection(global_allocator: std.mem.Allocator, connection: std.net.Server.Connection, db: *DB) void {
        defer connection.stream.close();

        var read_buffer: [4096]u8 = undefined;
        var http_server = http.Server.init(connection, &read_buffer);

        while (http_server.state == .ready) {
            // Scope arena to individual request to ensure cleanup
            var arena = std.heap.ArenaAllocator.init(global_allocator);
            defer arena.deinit();
            const arena_allocator = arena.allocator();

            var req = http_server.receiveHead() catch |err| {
                if (err == http.Server.ReceiveHeadError.HttpConnectionClosing) return;
                std.debug.print("Error receiving head: {}\n", .{err});
                return;
            };

            // Initialize components for this request using arena allocator
            var user_repo = UserRepository.init(db);
            var task_repo = TaskRepository.init(db);
            var user_service = UserService.init(&user_repo, arena_allocator);
            var task_service = TaskService.init(&task_repo, arena_allocator);
            var handler = Handler.init(arena_allocator, &user_service, &task_service);
            var router = Router.init(&handler);

            // Handle request
            router.dispatch(&req) catch |err| {
                std.debug.print("Error dispatching: {}\n", .{err});
                if (req.state == .waited) {
                     _ = req.respond("Internal Server Error", .{ .status = .internal_server_error }) catch {};
                }
            };

            // Arena deinit happens here, freeing all resources for this request
        }
    }
};
