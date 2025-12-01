# How to Run Lua (OpenResty) Implementation

## Prerequisites
*   **OpenResty:** You must have OpenResty installed (which includes Nginx and LuaJIT).
*   **SQLite3:** The shared library `libsqlite3.so` (or `.dylib` on macOS) must be available in the system library path.

## Installation

1.  **Clone the repository.**
2.  **Navigate to `LuaOpenResty/`.**

## Running the Server

OpenResty runs relative to a prefix. We will treat the `LuaOpenResty` directory as the prefix.

```bash
# Assuming you are in the LuaOpenResty directory
mkdir -p logs
openresty -p `pwd` -c conf/nginx.conf
```

Or if `openresty` is not in PATH, use the full path to the binary.

The server will start on port `8080`.

## Stopping the Server

```bash
openresty -p `pwd` -c conf/nginx.conf -s stop
```

## Running Tests

Tests use `busted`.

```bash
# Install busted via luarocks if needed
luarocks install busted

# Run tests
busted tests/
```

## Database

The database file `magnetar.db` will be automatically created in the root directory upon the first request (or server start).
