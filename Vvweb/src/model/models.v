module model

// User represents a user in the system.
pub struct User {
pub mut:
	id       int    [primary; sql: serial; json: "id"]
	username string [unique; json: "username"]
	email    string [unique; json: "email"]
}

// Task represents a task in the system.
pub struct Task {
pub mut:
	id          int    [primary; sql: serial; json: "id"]
	user_id     int    [json: "user_id"]
	title       string [json: "title"]
	description string [json: "description"]
	done        bool   [json: "done"]
}

// CreateUserRequest for creating a user
pub struct CreateUserRequest {
pub:
	username string [json: "username"]
	email    string [json: "email"]
}

// UpdateUserRequest for updating a user
pub struct UpdateUserRequest {
pub:
	username string [json: "username"]
}

// CreateTaskRequest for creating a task
pub struct CreateTaskRequest {
pub:
	title       string [json: "title"]
	description string [json: "description"]
}

// UpdateTaskRequest for updating a task
pub struct UpdateTaskRequest {
pub:
	title       string [json: "title"]
	description string [json: "description"]
}

// TaskDoneRequest for patching a task
pub struct TaskDoneRequest {
pub:
	done bool [json: "done"]
}
