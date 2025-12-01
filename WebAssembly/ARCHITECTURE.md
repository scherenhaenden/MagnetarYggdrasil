# Architecture

## Layered Architecture

### 1. Handlers (`handlers.rs`)
- **Responsibility**: HTTP Adapter.
- **Input**: `spin_sdk::http::Request`.
- **Output**: `spin_sdk::http::Response`.
- **Dependencies**: `Service`.

### 2. Service (`service.rs`)
- **Responsibility**: Business Logic.
- **Input**: DTOs / Domain Objects.
- **Output**: Domain Objects / Results.
- **Dependencies**: `Repository`.

### 3. Repository (`repository.rs`)
- **Responsibility**: Data Access.
- **Input**: Domain Objects / IDs.
- **Output**: Domain Objects / Database Rows.
- **Dependencies**: `spin_sdk::sqlite`.

### 4. Models (`models.rs`)
- **Responsibility**: Data Transfer and Domain definitions.

## Database
- **Technology**: SQLite (via `spin_sdk::sqlite`).
- **Schema Management**: Auto-migration on startup.
