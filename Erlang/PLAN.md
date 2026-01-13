# Plan

1.  *Setup Erlang environment.*
    - Install Erlang/OTP and Rebar3.
    - Initialize project with `rebar3 new app magnetar`.
2.  *Implement Database Layer.*
    - Use `esqlite` for SQLite interaction.
    - Create `magnetar_db` GenServer to handle DB connections and queries.
    - Implement schema creation and CRUD operations.
3.  *Implement Service Layer.*
    - Create `magnetar_service` to act as a bridge between Handlers and DB.
4.  *Implement Web Layer.*
    - Use `cowboy` for HTTP server.
    - Implement handlers for Users, Tasks, and Health.
    - Use `jsx` for JSON encoding/decoding.
5.  *Testing.*
    - Write Common Test suite in `test/magnetar_SUITE.erl`.
    - Cover all endpoints.
6.  *Dockerize.*
    - Create `Dockerfile` for multi-stage build.
