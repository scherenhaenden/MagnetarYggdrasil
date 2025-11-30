module service;

import model;
import repository;
import std.typecons;
import std.exception;

class ServiceException : Exception {
    this(string msg) { super(msg); }
}

class NotFoundException : ServiceException {
    this(string msg) { super(msg); }
}

// User Service

User[] getAllUsersService() {
    return getAllUsers();
}

User getUserService(long id) {
    auto user = getUserById(id);
    if (user.isNull) throw new NotFoundException("User not found");
    return user.get();
}

User createUserService(CreateUserRequest req) {
    if (req.name == "" || req.email == "") {
         throw new ServiceException("Name and email are required");
    }
    // We might want to check for email uniqueness here or handle DB error
    long id = createUser(req.name, req.email);
    return User(id, req.name, req.email);
}

User updateUserService(long id, UpdateUserRequest req) {
    auto user = getUserById(id);
    if (user.isNull) throw new NotFoundException("User not found");

    updateUser(id, req.name, req.email);

    // Return updated user
    auto updated = getUserById(id);
    return updated.get();
}

void deleteUserService(long id) {
    if (!userExists(id)) throw new NotFoundException("User not found");
    deleteUser(id);
}

// Task Service

Task[] getTasksByUserService(long userId) {
    if (!userExists(userId)) throw new NotFoundException("User not found");
    return getTasksByUserId(userId);
}

Task getTaskService(long id) {
    auto task = getTaskById(id);
    if (task.isNull) throw new NotFoundException("Task not found");
    return task.get();
}

Task createTaskService(long userId, CreateTaskRequest req) {
    if (!userExists(userId)) throw new NotFoundException("User not found");
    if (req.title == "") throw new ServiceException("Title is required");

    long id = createTask(userId, req.title, req.description);
    return Task(id, userId, req.title, req.description, false);
}

Task updateTaskService(long id, UpdateTaskRequest req) {
    auto task = getTaskById(id);
    if (task.isNull) throw new NotFoundException("Task not found");

    // Pass nullable bool
    updateTask(id, req.title, req.description, req.done);

    return getTaskById(id).get();
}

Task markTaskDoneService(long id) {
    auto task = getTaskById(id);
    if (task.isNull) throw new NotFoundException("Task not found");

    markTaskDone(id);
    return getTaskById(id).get();
}

void deleteTaskService(long id) {
    auto task = getTaskById(id);
    if (task.isNull) throw new NotFoundException("Task not found");
    deleteTask(id);
}

version(unittest) {
    import db;
    import std.file;

    // We can't easily mock the DB in this architecture without Dependency Injection
    // or using a test database.
    // For unit tests of the service layer, we ideally want to mock repository.
    // Since repository functions are global functions here (simple module approach),
    // mocking is harder.
    // So we will do integration tests using a test DB.

    shared static this() {
        // This runs once for the process.
        // We initialize a test DB.
        if (exists("test_service.db")) remove("test_service.db");
        initDB("test_service.db");
    }

    unittest {
        import std.stdio;
        writeln("Running Service Tests...");

        // Clear tables
        getDB().run("DELETE FROM tasks");
        getDB().run("DELETE FROM users");

        // Test User Service
        auto req = CreateUserRequest("Test User", "test@example.com");
        auto user = createUserService(req);
        assert(user.name == "Test User");
        assert(user.email == "test@example.com");
        assert(user.id != 0);

        auto fetched = getUserService(user.id);
        assert(fetched.name == "Test User");

        auto updateReq = UpdateUserRequest("Updated Name");
        auto updated = updateUserService(user.id, updateReq);
        assert(updated.name == "Updated Name");
        assert(updated.email == "test@example.com");

        // Test Task Service
        auto taskReq = CreateTaskRequest("My Task", "Desc");
        auto task = createTaskService(user.id, taskReq);
        assert(task.title == "My Task");
        assert(task.user_id == user.id);
        assert(task.done == false);

        auto tasks = getTasksByUserService(user.id);
        assert(tasks.length == 1);

        auto updateTaskReq = UpdateTaskRequest("New Title");
        auto updatedTask = updateTaskService(task.id, updateTaskReq);
        assert(updatedTask.title == "New Title");

        auto doneTask = markTaskDoneService(task.id);
        assert(doneTask.done == true);

        deleteTaskService(task.id);
        bool taskDeleted = false;
        try {
            getTaskService(task.id);
        } catch (NotFoundException e) {
            taskDeleted = true;
        }
        assert(taskDeleted);

        deleteUserService(user.id);
        bool userDeleted = false;
        try {
            getUserService(user.id);
        } catch (NotFoundException e) {
            userDeleted = true;
        }
        assert(userDeleted);
    }
}
