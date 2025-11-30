import Fluent
import Vapor

final class User: Model, Content {
    static let schema = "users"

    @ID(key: .id)
    var id: UUID?

    @Field(key: "name")
    var name: String

    @Field(key: "email")
    var email: String

    @Children(for: \.$user)
    var tasks: [Task]

    init() { }

    init(id: UUID? = nil, name: String, email: String) {
        self.id = id
        self.name = name
        self.email = email
    }
}

// DTOs
struct CreateUserDTO: Content {
    let name: String
    let email: String
}

struct UpdateUserDTO: Content {
    let name: String?
    let email: String?
}
