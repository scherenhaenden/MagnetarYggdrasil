# Run Swift (Vapor) Implementation

This implementation uses **Swift 5.9+** and **Vapor 4**.

## Prerequisites

- Swift 5.9 or later toolchain (available on macOS, Linux, and Windows)
- SQLite3 (usually available on system, Vapor includes driver)

## Installation

No special installation is needed other than having Swift installed. Dependencies are managed by Swift Package Manager (`Package.swift`).

## Running the Application

To run the application, execute:

```bash
swift run App
```

This will:
1.  Resolve and fetch dependencies.
2.  Build the application.
3.  Run the server on `http://localhost:8080`.
4.  Auto-create the `db.sqlite` database file and run migrations.

## Running Tests

To run the tests, execute:

```bash
swift test
```

## Structure

*   `Package.swift`: Dependencies and targets.
*   `Sources/App/Models`: Database models (User, Task) and DTOs.
*   `Sources/App/Controllers`: API handlers (UserController, TaskController).
*   `Sources/App/Migrations`: Database schema migrations.
*   `Sources/App/configure.swift`: Application configuration (DB, routes).
*   `Tests/AppTests`: Integration tests.
