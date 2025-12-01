# Architecture

## Overview
The application follows a strict layered architecture to separate concerns.

### Layers

1.  **Handler Layer (`clojure_ring.handler`)**
    -   Defines Routes using Reitit.
    -   Parses HTTP requests (JSON body, Path params).
    -   Calls Service layer.
    -   Formats HTTP responses.
    -   **No business logic allowed.**

2.  **Service Layer (`clojure_ring.service`)**
    -   Contains business logic.
    -   Validates inputs.
    -   Calls Repository layer.
    -   **No HTTP knowledge allowed.**

3.  **Repository Layer (`clojure_ring.repository`)**
    -   Executes SQL queries using `next.jdbc`.
    -   Maps SQL results to Clojure maps.
    -   **No business logic allowed.**

4.  **Database Layer (`clojure_ring.db`)**
    -   Manages SQLite connection source.
    -   Handles schema initialization.
