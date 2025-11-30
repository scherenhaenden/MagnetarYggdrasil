import Fluent

struct CreateTask: AsyncMigration {
    func prepare(on database: Database) async throws {
        try await database.schema("tasks")
            .id()
            .field("user_id", .uuid, .required, .references("users", "id"))
            .field("title", .string, .required)
            .field("description", .string, .required)
            .field("is_done", .bool, .required)
            .create()
    }

    func revert(on database: Database) async throws {
        try await database.schema("tasks").delete()
    }
}
