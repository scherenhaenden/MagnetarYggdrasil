@testable import App
import XCTVapor

final class AppTests: XCTestCase {
    var app: Application!

    override func setUp() async throws {
        app = Application(.testing)
        try await configure(app)
        try await app.autoMigrate()
    }

    override func tearDown() async throws {
        try await app.autoRevert()
        app.shutdown()
    }

    func testHealth() async throws {
        try app.test(.GET, "health", afterResponse: { res in
            XCTAssertEqual(res.status, .ok)
        })
    }

    func testUserCRUD() async throws {
        // Create User
        let userDTO = CreateUserDTO(name: "Test User", email: "test@example.com")
        var userID: UUID?

        try app.test(.POST, "users", beforeRequest: { req in
            try req.content.encode(userDTO)
        }, afterResponse: { res in
            XCTAssertEqual(res.status, .ok)
            let user = try res.content.decode(User.self)
            XCTAssertEqual(user.name, userDTO.name)
            XCTAssertEqual(user.email, userDTO.email)
            XCTAssertNotNil(user.id)
            userID = user.id
        })

        guard let createdUserID = userID else {
            XCTFail("User ID was nil")
            return
        }

        // Get User
        try app.test(.GET, "users/\(createdUserID)", afterResponse: { res in
            XCTAssertEqual(res.status, .ok)
            let user = try res.content.decode(User.self)
            XCTAssertEqual(user.id, createdUserID)
        })

        // List Users
        try app.test(.GET, "users", afterResponse: { res in
             XCTAssertEqual(res.status, .ok)
             let users = try res.content.decode([User].self)
             XCTAssertTrue(users.count >= 1)
        })

        // Update User
        let updateDTO = UpdateUserDTO(name: "Updated Name", email: "updated@example.com")
        try app.test(.PUT, "users/\(createdUserID)", beforeRequest: { req in
            try req.content.encode(updateDTO)
        }, afterResponse: { res in
            XCTAssertEqual(res.status, .ok)
            let user = try res.content.decode(User.self)
            XCTAssertEqual(user.name, updateDTO.name)
            XCTAssertEqual(user.email, updateDTO.email)
        })

        // Delete User
        try app.test(.DELETE, "users/\(createdUserID)", afterResponse: { res in
             XCTAssertEqual(res.status, .noContent)
        })

        // Verify Deletion
        try app.test(.GET, "users/\(createdUserID)", afterResponse: { res in
             XCTAssertEqual(res.status, .notFound)
        })
    }

    func testTaskCRUD() async throws {
         // Create User
        let userDTO = CreateUserDTO(name: "Task User", email: "task@example.com")
        var userID: UUID?

        try app.test(.POST, "users", beforeRequest: { req in
            try req.content.encode(userDTO)
        }, afterResponse: { res in
            let user = try res.content.decode(User.self)
            userID = user.id
        })

        guard let createdUserID = userID else { return }

        // Create Task
        let taskDTO = CreateTaskDTO(title: "My Task", description: "Do something")
        var taskID: UUID?

        try app.test(.POST, "users/\(createdUserID)/tasks", beforeRequest: { req in
             try req.content.encode(taskDTO)
        }, afterResponse: { res in
            XCTAssertEqual(res.status, .ok)
            let task = try res.content.decode(Task.self)
            XCTAssertEqual(task.title, taskDTO.title)
            XCTAssertEqual(task.description, taskDTO.description)
            XCTAssertFalse(task.isDone)
            taskID = task.id
        })

        guard let createdTaskID = taskID else {
             XCTFail("Task ID was nil")
             return
        }

        // Get Task
        try app.test(.GET, "tasks/\(createdTaskID)", afterResponse: { res in
            XCTAssertEqual(res.status, .ok)
            let task = try res.content.decode(Task.self)
            XCTAssertEqual(task.id, createdTaskID)
        })

        // List Tasks for User
        try app.test(.GET, "users/\(createdUserID)/tasks", afterResponse: { res in
            XCTAssertEqual(res.status, .ok)
            let tasks = try res.content.decode([Task].self)
            XCTAssertEqual(tasks.count, 1)
            XCTAssertEqual(tasks.first?.id, createdTaskID)
        })

        // Update Task
        let updateDTO = UpdateTaskDTO(title: "Updated Title", description: "Updated Desc", is_done: nil)
        try app.test(.PUT, "tasks/\(createdTaskID)", beforeRequest: { req in
            try req.content.encode(updateDTO)
        }, afterResponse: { res in
            XCTAssertEqual(res.status, .ok)
            let task = try res.content.decode(Task.self)
            XCTAssertEqual(task.title, updateDTO.title)
            XCTAssertEqual(task.description, updateDTO.description)
        })

        // Mark Done
        try app.test(.PATCH, "tasks/\(createdTaskID)/done", afterResponse: { res in
             XCTAssertEqual(res.status, .ok)
             let task = try res.content.decode(Task.self)
             XCTAssertTrue(task.isDone)
        })

        // Delete Task
        try app.test(.DELETE, "tasks/\(createdTaskID)", afterResponse: { res in
             XCTAssertEqual(res.status, .noContent)
        })

        // Verify Task Deletion
         try app.test(.GET, "tasks/\(createdTaskID)", afterResponse: { res in
             XCTAssertEqual(res.status, .notFound)
        })
    }
}
