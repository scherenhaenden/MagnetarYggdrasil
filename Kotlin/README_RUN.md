# MagnetarYggdrasil - Kotlin (Ktor)

## Build and Run

This implementation uses **Kotlin**, **Ktor**, and **Exposed** with **SQLite**.

### Prerequisites

- JDK 21+

### Run

```bash
./gradlew run
```

The server will start on `http://0.0.0.0:8080`.

### Database

The application uses `magnetar.db` (SQLite) in the root of the project directory. It is automatically created on the first run.

### Endpoints

- **Users**
    - `POST /users` - Create user
    - `GET /users` - List all users
    - `GET /users/{id}` - Get user by ID
    - `PUT /users/{id}` - Update user
    - `DELETE /users/{id}` - Delete user
- **Tasks**
    - `POST /users/{id}/tasks` - Create task for user
    - `GET /users/{id}/tasks` - List tasks for user
    - `GET /tasks/{tid}` - Get task by ID
    - `PUT /tasks/{tid}` - Update task
    - `PATCH /tasks/{tid}/done` - Mark task as done
    - `DELETE /tasks/{tid}` - Delete task
- **Health**
    - `GET /health` - Health check

### Testing

```bash
./gradlew test
```
