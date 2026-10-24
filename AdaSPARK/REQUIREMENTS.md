# Requirements for AdaSPARK

## Functional Requirements
*   **Must-Have:**
    *   Expose JSON HTTPS endpoints.
    *   `GET /health`
    *   `POST /users`, `GET /users`
    *   `GET /users/:id`, `PUT /users/:id`, `DELETE /users/:id`
    *   `POST /users/:id/tasks`, `GET /users/:id/tasks`
    *   `GET /tasks/:id`, `PUT /tasks/:id`, `DELETE /tasks/:id`
    *   `PATCH /tasks/:id/done`
    *   Use SQLite database.

## Non-Functional Requirements
*   **Must-Have:**
    *   Adhere to Magnetar Canonical Project Model.
    *   100% Test Coverage.
    *   Written in Ada/SPARK.
