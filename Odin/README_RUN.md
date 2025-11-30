# Odin Implementation

## How to Run

1.  Ensure you have `odin` installed.
2.  Ensure you have `sqlite3` installed on your system (or `libsqlite3`).
    *   **Linux:** `sudo apt install libsqlite3-dev`
    *   **macOS:** Installed by default, or `brew install sqlite`
    *   **Windows:** You might need to place `sqlite3.lib` and `sqlite3.dll` in the directory or link against them.

### Running

```bash
odin run . -out:magnetar
```

### Testing

```bash
odin test tests
```

## Structure

*   `sqlite/`: Manual bindings for SQLite3 C API.
*   `server/`: Manual TCP server and HTTP parsing.
*   `app/`: Application business logic (Models, Handlers, DB).
*   `main.odin`: Entry point.

## Notes

*   This implementation uses a manual TCP listener (`core:net`) and manual HTTP parsing.
*   JSON handling is done using `core:encoding/json` and some string manipulation.
*   SQLite is accessed via C bindings.
*   The server is single-threaded (blocking) for simplicity, as per "manual server" which can mean "from scratch" but could be optimized with threads.
