const std = @import("std");
const UserRepository = @import("../repository/user_repository.zig").UserRepository;
const User = @import("../models/user.zig").User;
const CreateUserRequest = @import("../models/user.zig").CreateUserRequest;
const UpdateUserRequest = @import("../models/user.zig").UpdateUserRequest;

pub const UserService = struct {
    repo: *UserRepository,
    allocator: std.mem.Allocator,

    pub fn init(repo: *UserRepository, allocator: std.mem.Allocator) UserService {
        return UserService{
            .repo = repo,
            .allocator = allocator,
        };
    }

    pub fn createUser(self: *UserService, req: CreateUserRequest) !User {
        // Validation could go here
        return self.repo.create(req.name, req.email, self.allocator);
    }

    pub fn getAllUsers(self: *UserService) ![]User {
        return self.repo.getAll(self.allocator);
    }

    pub fn getUserById(self: *UserService, id: i64) !?User {
        return self.repo.getById(id, self.allocator);
    }

    pub fn updateUser(self: *UserService, id: i64, req: UpdateUserRequest) !?User {
        return self.repo.update(id, req.name, req.email, self.allocator);
    }

    pub fn deleteUser(self: *UserService, id: i64) !bool {
        return self.repo.delete(id);
    }
};
