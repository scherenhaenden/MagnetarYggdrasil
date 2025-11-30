# Requirements

## API Interface
- `POST /users`: Create user.
- `GET /users`: List users.
- `GET /users/{id}`: Get user.
- `PUT /users/{id}`: Update user.
- `DELETE /users/{id}`: Delete user.
- `POST /users/{id}/tasks`: Create task for user.
- `GET /users/{id}/tasks`: List tasks for user.
- `GET /tasks/{id}`: Get task.
- `PUT /tasks/{id}`: Update task.
- `PATCH /tasks/{id}/done`: Mark task as done.
- `DELETE /tasks/{id}`: Delete task.
- `GET /health`: System health.

## Database
- SQLite3
- Auto-creation of schema.
- Foreign Keys enabled.

## Architecture
- Layered: Handler -> Service -> Repository.
