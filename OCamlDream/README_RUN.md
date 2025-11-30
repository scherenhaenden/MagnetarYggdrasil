# How to Run OCaml (Dream) Implementation

## Prerequisites
- **OCaml:** Install via opam (OCaml Package Manager).
- **Opam:** `opam init`
- **Dune:** `opam install dune`

## Installation

1.  **Install Dependencies:**
    ```bash
    opam install dream caqti caqti-driver-sqlite3 ppx_yojson_conv ppx_deriving lwt alcotest
    ```

2.  **Build:**
    ```bash
    dune build
    ```

## Running the Application
```bash
dune exec bin/main.exe
```
The server will start at `http://localhost:8080`.

## Running Tests
```bash
dune runtest
```

## API Endpoints
See the main `README.md` or `REQUIREMENTS.md` for the full list of endpoints.
