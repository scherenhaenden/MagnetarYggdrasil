# Erlang (Cowboy) Implementation

This is the Erlang implementation of the MagnetarYggdrasil project using the Cowboy web server and `esqlite` for SQLite database interactions.

## Prerequisites

*   Erlang/OTP 24+
*   Rebar3

## Running

1.  Compile the project:
    ```bash
    rebar3 compile
    ```

2.  Run the shell:
    ```bash
    rebar3 shell
    ```
    This will start the application on port 8080.

## Testing

Run the Common Test suite:
```bash
rebar3 ct
```

## Structure

*   `src/magnetar_app.erl`: Application entry point.
*   `src/magnetar_sup.erl`: Supervisor.
*   `src/magnetar_db.erl`: Database GenServer and SQLite interactions.
*   `src/magnetar_service.erl`: Service layer facade.
*   `src/magnetar_*_handler.erl`: Cowboy handlers for HTTP endpoints.
