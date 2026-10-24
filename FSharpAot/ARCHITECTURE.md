# Architecture of F# (.NET AOT)

## Architecture Diagram
```mermaid
graph TD
    Client[HTTP Client] --> Handlers[Handlers (Minimal APIs)]
    Handlers --> Service[Service Layer]
    Service --> Repository[Repository Layer]
    Repository --> DB[(SQLite Database)]
```

## Component Descriptions

### Handlers (Controllers)
*   **Responsibility:** Handle HTTP requests, parse JSON, validate input format, call Service, return HTTP responses.
*   **Technology:** ASP.NET Core Minimal APIs (`MapGet`, `MapPost`, etc.).

### Service Layer
*   **Responsibility:** Business logic, domain validation, orchestration.
*   **Technology:** F# Modules/Classes.

### Repository Layer
*   **Responsibility:** Data access, SQL queries, mapping to Domain Models.
*   **Technology:** `Microsoft.Data.Sqlite` (Raw ADO.NET) for AOT compatibility.

### Models
*   **Responsibility:** Data structures for Users and Tasks.
*   **Technology:** F# Records.

### Database
*   **Responsibility:** Persistent storage.
*   **Technology:** SQLite3.
