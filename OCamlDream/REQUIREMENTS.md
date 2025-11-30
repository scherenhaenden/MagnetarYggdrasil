# Requirements

(Copied from Root/REQUIREMENTS.md)

## API Interface
- `POST /users`: Create user.
- `GET /users`: List users.
- `GET /users/:id`: Get user.
- `PUT /users/:id`: Update user.
- `DELETE /users/:id`: Delete user.
- `POST /users/:id/tasks`: Create task.
- `GET /users/:id/tasks`: List user tasks.
- `GET /tasks/:id`: Get task.
- `PUT /tasks/:id`: Update task.
- `PATCH /tasks/:id/done`: Mark task done.
- `DELETE /tasks/:id`: Delete task.
- `GET /health`: System status.

## Database
- SQLite3.
- Tables: `users`, `tasks`.
- Foreign keys enabled.

## Architecture
- Handlers -> Service -> Repository -> DB.
