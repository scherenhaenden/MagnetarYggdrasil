# Zig std.http Implementation

## Prerequisites
- Zig 0.13.0 or later.
- Internet connection to fetch dependencies (sqlite).

## How to Run

1.  Navigate to the directory:
    ```bash
    cd Zig
    ```

2.  Run the application:
    ```bash
    zig build run
    ```

    *Note: The first run might fail with a checksum mismatch for dependencies. If this happens, please run the command provided in the error message to update the hash in `build.zig.zon`.*

    This will:
    - Download dependencies.
    - Compile the project.
    - Create the SQLite database (`database.db`) if it doesn't exist.
    - Start the server on `http://127.0.0.1:8080`.

## Testing

To run the tests:

```bash
zig build test
```

## Structure

- `src/main.zig`: Entry point.
- `src/api/`: HTTP server, routing, and handlers.
- `src/service/`: Business logic.
- `src/repository/`: Database access.
- `src/storage/`: Database connection and migrations.
- `src/models/`: Data structures.
