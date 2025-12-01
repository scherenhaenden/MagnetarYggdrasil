# Architecture of Lua (OpenResty)

## High-Level Architecture

Request -> Nginx (OpenResty) -> Lua Controller -> Lua Service -> Lua Repository -> SQLite3

## Components

*   **OpenResty:** Web server and Lua environment.
*   **Controller Layer:** Handles HTTP request/response, validation errors.
*   **Service Layer:** Business logic, orchestrates repository calls.
*   **Repository Layer:** Raw SQL execution using `lua-resty-ljsqlite3` or similar FFI binding.
*   **Models:** Lua tables representing Users and Tasks.
