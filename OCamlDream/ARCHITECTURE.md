# Architecture

## Layered Design

1.  **Handlers (`lib/handlers.ml`)**:
    - Receives HTTP requests (Dream).
    - Parses JSON bodies.
    - Calls Service layer.
    - Returns HTTP responses (JSON).

2.  **Service (`lib/service.ml`)**:
    - Contains business logic.
    - Validates input.
    - Calls Repository layer.

3.  **Repository (`lib/repository.ml`)**:
    - Executes SQL queries via Caqti.
    - Maps database rows to OCaml records.

4.  **Database (`lib/database.ml`)**:
    - Manages SQLite connection pool.
    - Handles schema creation.

5.  **Models (`lib/models.ml`)**:
    - OCaml records with `ppx_yojson_conv` for JSON serialization/deserialization.
