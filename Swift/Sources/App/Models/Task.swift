import Fluent
import Vapor

final class Task: Model, Content {
    static let schema = "tasks"

    @ID(key: .id)
    var id: UUID?

    @Parent(key: "user_id")
    var user: User

    @Field(key: "title")
    var title: String

    @Field(key: "description")
    var description: String

    @Field(key: "is_done")
    var isDone: Bool

    init() { }

    init(id: UUID? = nil, userID: User.IDValue, title: String, description: String, isDone: Bool = false) {
        self.id = id
        self.$user.id = userID
        self.title = title
        self.description = description
        self.isDone = isDone
    }
}

// DTOs
struct CreateTaskDTO: Content {
    let title: String
    let description: String
}

struct UpdateTaskDTO: Content {
    let title: String?
    let description: String?
    let is_done: Bool?
}
