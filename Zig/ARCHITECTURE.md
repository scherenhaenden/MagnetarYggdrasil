# Architecture of Zig

## Overview
The application follows a layered architecture to ensure separation of concerns and testability.

## Architecture Diagram

Request -> [API / Handlers] -> [Service Layer] -> [Repository Layer] -> [SQLite Database]

## Components

### 1. API / Handlers (`src/api/`)
-   **Responsibility:** Handle HTTP requests, parse JSON, validate input, and call services.
-   **Technology:** Zig `std.http`.

### 2. Service Layer (`src/service/`)
-   **Responsibility:** Implement business logic and orchestrate data flow.
-   **Technology:** Pure Zig structs/functions.

### 3. Repository Layer (`src/repository/`)
-   **Responsibility:** Abstract database interactions.
-   **Technology:** `vrischmann/zig-sqlite` wrapper.

### 4. Database (`src/storage/`)
-   **Responsibility:** Persistent storage.
-   **Technology:** SQLite3 (protected by Mutex for concurrency).
