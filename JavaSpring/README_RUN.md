# Java (Spring Boot) Implementation

This is the Java (Spring Boot) implementation for the MagnetarYggdrasil project.

## Requirements

*   Java 17 or higher
*   Maven

## Running the Application

To run the application, use the Maven wrapper (or your installed Maven):

```bash
cd JavaSpring
mvn spring-boot:run
```

The application will start on port 8080.
The SQLite database `magnetaryggdrasil.db` will be automatically created in the root of the `JavaSpring` directory.

## Testing

To run the tests:

```bash
cd JavaSpring
mvn test
```

## Endpoints

### Users

*   `POST /users` - Create a user
*   `GET /users` - List all users
*   `GET /users/{id}` - Get a user
*   `PUT /users/{id}` - Update a user
*   `DELETE /users/{id}` - Delete a user

### Tasks

*   `POST /users/{id}/tasks` - Create a task for a user
*   `GET /users/{id}/tasks` - List tasks for a user
*   `GET /tasks/{tid}` - Get a task
*   `PUT /tasks/{tid}` - Update a task
*   `PATCH /tasks/{tid}/done` - Mark a task as done
*   `DELETE /tasks/{tid}` - Delete a task

### Health

*   `GET /health` - Health check
