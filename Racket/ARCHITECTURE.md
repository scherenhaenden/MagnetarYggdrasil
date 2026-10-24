# Architecture of MagnetarYggdrasil - Racket

## Architecture Diagram
[Client] -> [Handler (Web Server)] -> [Service (Logic)] -> [Repository (DB)] -> [SQLite]

## Component Descriptions
*   **Handler (main.rkt/handlers.rkt):** Manages HTTP requests, routing, and responses using `web-server/servlet-env` or similar.
*   **Service (service.rkt):** Contains business logic and orchestration.
*   **Repository (repository.rkt):** Handles direct database interactions using `db` library.
*   **Models:** Data structures representing Users and Tasks.
