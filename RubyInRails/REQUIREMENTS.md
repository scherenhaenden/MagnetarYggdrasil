# Requirements for MagnetarYggdrasil - Ruby (Rails)

## Functional Requirements (Must-Have)

### Users Resource
*   `POST /users`: Create user (username, email).
*   `GET /users`: List users.
*   `GET /users/{id}`: Get user details.
*   `PUT /users/{id}`: Update user.
*   `DELETE /users/{id}`: Delete user.

### Tasks Resource
*   `POST /users/{id}/tasks`: Create task for user.
*   `GET /users/{id}/tasks`: List tasks for user.
*   `GET /tasks/{id}`: Get task details.
*   `PUT /tasks/{id}`: Update task.
*   `PATCH /tasks/{id}/done`: Mark task as done.
*   `DELETE /tasks/{id}`: Delete task.

### System
*   `GET /health`: Health check.

## Non-Functional Requirements
*   **Database:** SQLite3 with Foreign Keys enabled.
*   **Architecture:** Layered (Controller -> Service -> Repository -> Model).
*   **Testing:** 100% Code Coverage.
