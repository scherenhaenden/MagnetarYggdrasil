# Requirements for MagnetarYggdrasil - Scala (Akka)

## Functional Requirements

### Users
-   **Must-Have**: Create User (`POST /users`)
-   **Must-Have**: Get All Users (`GET /users`)
-   **Must-Have**: Get User by ID (`GET /users/:id`)
-   **Must-Have**: Update User (`PUT /users/:id`)
-   **Must-Have**: Delete User (`DELETE /users/:id`)

### Tasks
-   **Must-Have**: Create Task for User (`POST /users/:id/tasks`)
-   **Must-Have**: Get All Tasks for User (`GET /users/:id/tasks`)
-   **Must-Have**: Get Task by ID (`GET /tasks/:id`)
-   **Must-Have**: Update Task (`PUT /tasks/:id`)
-   **Must-Have**: Mark Task as Done (`PATCH /tasks/:id/done`)
-   **Must-Have**: Delete Task (`DELETE /tasks/:id`)

## Non-Functional Requirements
-   **Must-Have**: Use Scala with Akka HTTP.
-   **Must-Have**: Use SQLite database.
-   **Must-Have**: 100% Test Coverage.
-   **Must-Have**: Follow Layered Architecture.
-   **Must-Have**: JSON responses.
