# Requirements for Bun Project

## Functional Requirements (Must-Have)

### User Management
1.  **Create User:** API must allow creating a user with `name` and `email`. Email must be unique.
2.  **Get User:** API must allow retrieving a user by ID.
3.  **List Users:** API must allow retrieving all users.
4.  **Update User:** API must allow updating a user's details.
5.  **Delete User:** API must allow deleting a user.

### Task Management
1.  **Create Task:** API must allow creating a task for a user.
2.  **Get Task:** API must allow retrieving a task by ID.
3.  **List User Tasks:** API must allow retrieving all tasks for a specific user.
4.  **Update Task:** API must allow updating a task.
5.  **Mark Task Done:** API must allow marking a task as done.
6.  **Delete Task:** API must allow deleting a task.

### General
1.  **Health Check:** `GET /health` endpoint returning 200 OK.
2.  **JSON API:** All endpoints must accept and return JSON.

## Non-Functional Requirements (Must-Have)
1.  **Database:** Must use SQLite.
2.  **Testing:** Must have 100% test coverage.
3.  **Architecture:** Must follow a layered architecture (Handler -> Service -> Repository).
4.  **Runtime:** Must run on Bun.
