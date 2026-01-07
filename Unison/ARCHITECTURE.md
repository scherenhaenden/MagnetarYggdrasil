# Architecture of Unison

## Architecture Diagram
[Client] -> [Server (HTTP)] -> [Handler] -> [Service] -> [Repository] -> [SQLite DB]

## Component Descriptions
*   **Server:** Handles incoming HTTP requests and routes them to the appropriate handlers.
*   **Handler:** Parses requests, validates input, and invokes the service layer.
*   **Service:** Contains business logic and orchestrates data operations.
*   **Repository:** Abstract data access layer for SQLite interactions.
*   **Domain:** Defines core data models and types.
