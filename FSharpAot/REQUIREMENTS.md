# Requirements for F# (.NET AOT)

## Functional Requirements

### Users Resource (Must-Have)
*   `POST /users`: Create user (username, email).
*   `GET /users`: List all users.
*   `GET /users/{id}`: Get user by ID.
*   `PUT /users/{id}`: Update user (username).
*   `DELETE /users/{id}`: Delete user.

### Tasks Resource (Must-Have)
*   `POST /users/{id}/tasks`: Create task for user.
*   `GET /users/{id}/tasks`: List tasks for user.
*   `GET /tasks/{id}`: Get task by ID.
*   `PUT /tasks/{id}`: Update task (title, description).
*   `PATCH /tasks/{id}/done`: Mark task as done.
*   `DELETE /tasks/{id}`: Delete task.

### System (Must-Have)
*   `GET /health`: Return status and version.

## Non-Functional Requirements
*   **Performance:** High throughput, low latency (comparable to Go/Rust).
*   **Startup:** Fast startup time (Native AOT).
*   **Memory:** Low memory footprint.
*   **Database:** SQLite3 with Foreign Keys enabled.
*   **Architecture:** Layered (Handlers -> Service -> Repository).
*   **Testing:** 100% Code Coverage.
