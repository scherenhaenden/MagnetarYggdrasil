# Architecture of V (vweb)

## Architecture Diagram

Request -> [API Layer (vweb Handlers)] -> [Service Layer] -> [Repository Layer] -> [SQLite Database]

## Component Descriptions

1.  **API Layer (`src/api`):**
    *   Handles HTTP requests using `vweb`.
    *   Parses JSON inputs.
    *   Formats JSON responses.
    *   Calls Service layer.
    *   No business logic.

2.  **Service Layer (`src/service`):**
    *   Implements business logic.
    *   Validates input data.
    *   Calls Repository layer.

3.  **Repository Layer (`src/repository`):**
    *   Executes SQL queries.
    *   Maps SQL rows to Domain Models.
    *   Manages database connections.

4.  **Models (`src/model`):**
    *   Structs representing Users and Tasks.
    *   JSON serialization tags.
