const std = @import("std");
const TaskRepository = @import("../repository/task_repository.zig").TaskRepository;
const Task = @import("../models/task.zig").Task;
const CreateTaskRequest = @import("../models/task.zig").CreateTaskRequest;
const UpdateTaskRequest = @import("../models/task.zig").UpdateTaskRequest;

pub const TaskService = struct {
    repo: *TaskRepository,
    allocator: std.mem.Allocator,

    pub fn init(repo: *TaskRepository, allocator: std.mem.Allocator) TaskService {
        return TaskService{
            .repo = repo,
            .allocator = allocator,
        };
    }

    pub fn createTask(self: *TaskService, user_id: i64, req: CreateTaskRequest) !Task {
        return self.repo.create(user_id, req.title, req.description, self.allocator);
    }

    pub fn getTasksByUserId(self: *TaskService, user_id: i64) ![]Task {
        return self.repo.getAllByUserId(user_id, self.allocator);
    }

    pub fn getTaskById(self: *TaskService, id: i64) !?Task {
        return self.repo.getById(id, self.allocator);
    }

    pub fn updateTask(self: *TaskService, id: i64, req: UpdateTaskRequest) !?Task {
        return self.repo.update(id, req.title, req.description, req.done, self.allocator);
    }

    pub fn markTaskDone(self: *TaskService, id: i64) !bool {
        return self.repo.markDone(id);
    }

    pub fn deleteTask(self: *TaskService, id: i64) !bool {
        return self.repo.delete(id);
    }
};
