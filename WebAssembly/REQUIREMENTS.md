# Requirements

## Functional Requirements
1.  **Users API**: CRUD for Users.
2.  **Tasks API**: CRUD for Tasks (nested under Users).
3.  **System API**: Health check.
4.  **Database**: SQLite3 storage.
5.  **Architecture**: Layered (Handler -> Service -> Repository).

## Non-Functional Requirements
1.  **Performance**: High throughput, low latency.
2.  **Scalability**: Stateless Wasm components allow easy scaling.
3.  **Portability**: Runs on any WASI-compliant runtime (Spin).
