# Zig Implementation

This directory contains the Zig implementation of the MagnetarYggdrasil benchmark project.

## Prerequisites

- [Zig](https://ziglang.org/) (latest stable, e.g., 0.12.0 or 0.13.0)
- SQLite3 development libraries (usually installed by default on many systems, or install via package manager)

## Running

To run the server:

```bash
zig build run
```

The server listens on `http://0.0.0.0:8080`.

## Testing

To run the tests:

```bash
zig build test
```

## Structure

- `src/main.zig`: Entry point and HTTP server.
- `src/handlers.zig`: HTTP request handlers.
- `src/service.zig`: Business logic.
- `src/repository.zig`: Database interaction using `zig-sqlite`.
- `src/db.zig`: Database connection and schema initialization.
- `src/models.zig`: Data structures.
- `src/tests.zig`: Integration tests.

## Dependencies

- `zig-sqlite` (vendored/fetched via `build.zig.zon`)
