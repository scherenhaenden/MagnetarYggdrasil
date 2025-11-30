# Requirements for JuliaGenie

## Functional Requirements

### 1. User Management (Must-Have)
*   Create User: `POST /users` (JSON, returns 201)
*   List Users: `GET /users` (JSON, returns 200)
*   Get User: `GET /users/{id}` (JSON, returns 200 or 404)
*   Update User: `PUT /users/{id}` (JSON, returns 200)
*   Delete User: `DELETE /users/{id}` (Returns 204)

### 2. Task Management (Must-Have)
*   Create Task: `POST /users/{id}/tasks` (JSON, returns 201)
*   List Tasks: `GET /users/{id}/tasks` (JSON, returns 200)
*   Get Task: `GET /tasks/{id}` (JSON, returns 200)
*   Update Task: `PUT /tasks/{id}` (JSON, returns 200)
*   Mark Done: `PATCH /tasks/{id}/done` (Returns 200)
*   Delete Task: `DELETE /tasks/{id}` (Returns 204)

### 3. System (Must-Have)
*   Health Check: `GET /health` (Returns 200, status "ok")

## Non-Functional Requirements

### 1. Database (Must-Have)
*   Use SQLite3.
*   Auto-create database and schema on startup.
*   Enforce Foreign Keys.

### 2. Architecture (Must-Have)
*   Layered Architecture: Controller -> Service -> Repository.
*   Separation of concerns.

### 3. Testing (Must-Have)
*   100% Code Coverage.
*   Unit, Integration, and E2E tests.

### 4. Governance (Must-Have)
*   Adherence to Magnetar Canonical Project Model.
