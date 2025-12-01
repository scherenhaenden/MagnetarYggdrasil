# Architecture of MagnetarYggdrasil - Crystal (Kemal)

## Diagram
`Request` -> `Controller` -> `Service` -> `Repository` -> `Database`

## Component Descriptions

### Controllers (Handlers)
Responsible for handling HTTP requests, parsing JSON, calling Services, and returning HTTP responses.
*   `UserController`: Handles `/users` endpoints.
*   `TaskController`: Handles `/tasks` endpoints.
*   `SystemController`: Handles `/health`.

### Services
Contains business logic.
*   `UserService`: User validation and logic.
*   `TaskService`: Task validation and logic.

### Repositories
Responsible for SQL queries and DB interaction.
*   `UserRepository`: CRUD for Users.
*   `TaskRepository`: CRUD for Tasks.

### Models
Plain objects/structs used to transfer data.
*   `User`
*   `Task`
