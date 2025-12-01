# Architecture of MagnetarYggdrasil - Scala (Akka)

## Architecture Diagram
`Routes` -> `Service` -> `Repository` -> `Database (SQLite)`

## Component Descriptions
-   **Routes (Handlers):** Define HTTP endpoints and map requests to Service calls. Uses Akka HTTP.
-   **Service:** Contains business logic.
-   **Repository:** Handles data access and persistence using Slick/JDBC.
-   **Database:** SQLite database storing `users` and `tasks`.
