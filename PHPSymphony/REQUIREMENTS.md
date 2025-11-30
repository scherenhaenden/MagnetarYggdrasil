# Requirements for PHPSymphony

## Functional Requirements

### 1. API Interface (Strict Compliance)

The application MUST expose the following HTTP JSON endpoints.

#### **Users Resource**
*   `POST /users`
    *   **Input:** JSON `{ "username": "string", "email": "string" }`
    *   **Output:** JSON `{ "id": integer, "username": "string", "email": "string" }` (HTTP 201)
    *   **Error:** HTTP 400 if invalid.
*   `GET /users`
    *   **Output:** JSON array of users `[ { ... }, { ... } ]` (HTTP 200)
*   `GET /users/{id}`
    *   **Output:** JSON user object (HTTP 200)
    *   **Error:** HTTP 404 if not found.
*   `PUT /users/{id}`
    *   **Input:** JSON `{ "username": "string" }` (Partial updates allowed)
    *   **Output:** JSON user object (HTTP 200)
*   `DELETE /users/{id}`
    *   **Output:** HTTP 204 No Content.

#### **Tasks Resource**
*   `POST /users/{id}/tasks`
    *   **Input:** JSON `{ "title": "string", "description": "string" }`
    *   **Output:** JSON task object `{ "id": integer, "user_id": integer, "title": "...", "description": "...", "done": boolean }` (HTTP 201)
*   `GET /users/{id}/tasks`
    *   **Output:** JSON array of tasks for that user (HTTP 200)
*   `GET /tasks/{id}`
    *   **Output:** JSON task object (HTTP 200)
*   `PUT /tasks/{id}`
    *   **Input:** JSON `{ "title": "...", "description": "..." }`
    *   **Output:** Updated task object (HTTP 200)
*   `PATCH /tasks/{id}/done`
    *   **Input:** None (or JSON `{ "done": true }`)
    *   **Output:** Updated task object with `done=true` (HTTP 200)
*   `DELETE /tasks/{id}`
    *   **Output:** HTTP 204 No Content.

#### **System**
*   `GET /health`
    *   **Output:** JSON `{ "status": "ok", "version": "1.0.0" }` (HTTP 200)

### 2. Database (SQLite)

*   **Engine:** SQLite3 MUST be used.
*   **Auto-Creation:** The application MUST check for the existence of the database file on startup. If missing, it MUST create it and apply the schema automatically.
*   **Foreign Keys:** Foreign Key constraints MUST be enabled.
*   **Schema:**
    *   `users` table: `id` (PK, Auto Inc), `username` (Text, Unique), `email` (Text, Unique).
    *   `tasks` table: `id` (PK, Auto Inc), `user_id` (FK -> users.id, On Delete Cascade), `title` (Text), `description` (Text), `done` (Boolean/Int).

## Non-Functional Requirements

### 1. Architecture

The code MUST follow a Layered Architecture:
*   **Controller Layer:** HTTP request/response handling. No business logic.
*   **Service Layer:** Business logic.
*   **Repository Layer:** Database interaction.
*   **Entities/DTOs:** Data transfer objects.

### 2. Testing

*   **Coverage:** 100% Code Coverage (Unit, Integration, E2E).
*   **Unit Tests:** Mock database/repository.
*   **Integration Tests:** Test full flow with in-memory DB.
*   **E2E Tests:** Test against live API.

### 3. Performance & Usage
*   Minimize dependencies where possible.
*   Ensure thread safety (though PHP is generally process-based per request).
