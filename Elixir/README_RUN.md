# Run Instructions for Elixir (Phoenix)

## Prerequisites

- **Elixir**: 1.14+
- **Erlang/OTP**: 25+
- **Mix**: Build tool shipped with Elixir

## Setup

1.  **Install dependencies**:
    ```bash
    mix deps.get
    ```

2.  **Setup the database**:
    ```bash
    mix ecto.setup
    ```
    This will create the SQLite database file and run migrations.

## Running the Server

Start the Phoenix server:

```bash
mix phx.server
```

The server will be available at `http://localhost:4000`.

## Running Tests

Run the test suite:

```bash
mix test
```

## Architecture

This implementation follows the layered architecture as requested:

-   **API Layer**: Phoenix Controllers (`MagnetarWeb.*Controller`)
-   **Service Layer**: Phoenix Contexts (`Magnetar.Accounts`, `Magnetar.Todos`)
-   **Repository Layer**: Ecto Repo (`Magnetar.Repo`) and Schemas (`Magnetar.Accounts.User`, `Magnetar.Todos.Task`)
-   **Storage Layer**: SQLite (`ecto_sqlite3`)

## Endpoints

-   `GET /health`: Health check
-   `GET /users`: List users
-   `POST /users`: Create user
-   `GET /users/:id`: Get user
-   `PUT /users/:id`: Update user
-   `DELETE /users/:id`: Delete user
-   `GET /users/:id/tasks`: List tasks for user
-   `POST /users/:id/tasks`: Create task for user
-   `GET /tasks/:id`: Get task
-   `PUT /tasks/:id`: Update task
-   `PATCH /tasks/:id/done`: Mark task as done
-   `DELETE /tasks/:id`: Delete task
