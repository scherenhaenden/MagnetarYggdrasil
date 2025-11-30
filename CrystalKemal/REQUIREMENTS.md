# Requirements for MagnetarYggdrasil - Crystal (Kemal)

## Functional Requirements

### Users Resource (Must-Have)
*   `POST /users`: Create user (JSON input, JSON output 201).
*   `GET /users`: List users (JSON output 200).
*   `GET /users/{id}`: Get user (JSON output 200, 404 if missing).
*   `PUT /users/{id}`: Update user (JSON input, JSON output 200).
*   `DELETE /users/{id}`: Delete user (204).

### Tasks Resource (Must-Have)
*   `POST /users/{id}/tasks`: Create task (JSON input, JSON output 201).
*   `GET /users/{id}/tasks`: List tasks for user (JSON output 200).
*   `GET /tasks/{id}`: Get task (JSON output 200).
*   `PUT /tasks/{id}`: Update task (JSON input, JSON output 200).
*   `PATCH /tasks/{id}/done`: Mark task done (JSON output 200).
*   `DELETE /tasks/{id}`: Delete task (204).

### System (Must-Have)
*   `GET /health`: Health check (JSON `{ "status": "ok", "version": "1.0.0" }`, 200).

## Non-Functional Requirements

### Database (Must-Have)
*   SQLite3 engine.
*   Auto-creation of DB file and schema on startup.
*   Foreign Keys enabled.

### Architecture (Must-Have)
*   Layered Architecture: Controllers -> Services -> Repositories -> Models.
*   No business logic in Controllers.
*   No HTTP knowledge in Repositories.

### Testing (Must-Have)
*   100% Code Coverage.
*   Unit, Integration, and E2E tests.
