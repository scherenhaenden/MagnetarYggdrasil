# Requirements for Lua (OpenResty)

## Functional Requirements (Must-Have)

### Users Resource
*   `POST /users`: Create user (username, email). Return 201.
*   `GET /users`: List users. Return 200.
*   `GET /users/{id}`: Get user by ID. Return 200 or 404.
*   `PUT /users/{id}`: Update user (username). Return 200.
*   `DELETE /users/{id}`: Delete user. Return 204.

### Tasks Resource
*   `POST /users/{id}/tasks`: Create task for user. Return 201.
*   `GET /users/{id}/tasks`: List tasks for user. Return 200.
*   `GET /tasks/{id}`: Get task by ID. Return 200.
*   `PUT /tasks/{id}`: Update task (title, description). Return 200.
*   `PATCH /tasks/{id}/done`: Mark task as done. Return 200.
*   `DELETE /tasks/{id}`: Delete task. Return 204.

### System
*   `GET /health`: Return status ok and version.

## Non-Functional Requirements (Must-Have)
*   **Database:** SQLite3.
*   **Auto-Creation:** DB created if missing.
*   **Foreign Keys:** Enabled.
*   **Architecture:** Layered (Controller, Service, Repository).
*   **Testing:** 100% coverage target.
