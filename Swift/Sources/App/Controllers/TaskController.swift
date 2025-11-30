import Fluent
import Vapor

struct TaskController: RouteCollection {
    func boot(routes: RoutesBuilder) throws {
        // /users/:id/tasks
        routes.group("users", ":userID", "tasks") { tasks in
            tasks.post(use: create)
            tasks.get(use: index)
        }

        // /tasks/:id
        routes.group("tasks", ":taskID") { task in
            task.get(use: show)
            task.put(use: update)
            task.delete(use: delete)
            task.patch("done", use: markDone)
        }
    }

    func index(req: Request) async throws -> [Task] {
        guard let userID = req.parameters.get("userID", as: UUID.self) else {
             throw Abort(.badRequest)
        }
        // Verify user exists
        guard let _ = try await User.find(userID, on: req.db) else {
             throw Abort(.notFound)
        }

        return try await Task.query(on: req.db)
            .filter(\.$user.$id == userID)
            .all()
    }

    func create(req: Request) async throws -> Task {
        guard let userID = req.parameters.get("userID", as: UUID.self) else {
            throw Abort(.badRequest)
        }
        // Verify user exists
        guard let _ = try await User.find(userID, on: req.db) else {
             throw Abort(.notFound)
        }

        let taskDto = try req.content.decode(CreateTaskDTO.self)
        let task = Task(userID: userID, title: taskDto.title, description: taskDto.description)
        try await task.save(on: req.db)
        return task
    }

    func show(req: Request) async throws -> Task {
        guard let task = try await Task.find(req.parameters.get("taskID"), on: req.db) else {
            throw Abort(.notFound)
        }
        return task
    }

    func update(req: Request) async throws -> Task {
        guard let task = try await Task.find(req.parameters.get("taskID"), on: req.db) else {
            throw Abort(.notFound)
        }
        let updateDto = try req.content.decode(UpdateTaskDTO.self)

        if let title = updateDto.title {
            task.title = title
        }
        if let description = updateDto.description {
            task.description = description
        }
        // This endpoint supports generic updates, but there is a specific one for done.
        // If is_done is passed here, we can update it too.
        if let isDone = updateDto.is_done {
            task.isDone = isDone
        }

        try await task.save(on: req.db)
        return task
    }

    func markDone(req: Request) async throws -> Task {
        guard let task = try await Task.find(req.parameters.get("taskID"), on: req.db) else {
            throw Abort(.notFound)
        }
        task.isDone = true
        try await task.save(on: req.db)
        return task
    }

    func delete(req: Request) async throws -> HTTPStatus {
        guard let task = try await Task.find(req.parameters.get("taskID"), on: req.db) else {
            throw Abort(.notFound)
        }
        try await task.delete(on: req.db)
        return .noContent
    }
}
