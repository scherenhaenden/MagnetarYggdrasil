# How to Run JuliaGenie

## Prerequisites
*   **Julia:** Version 1.9 or higher.
*   **Git**

## Setup
1.  Navigate to the project directory:
    ```bash
    cd JuliaGenie
    ```
2.  Install dependencies:
    ```bash
    julia --project=. -e 'using Pkg; Pkg.instantiate()'
    ```

## Running the Server
1.  Start the server:
    ```bash
    julia --project=. bin/server
    ```
    Or simply:
    ```bash
    julia --project=. -e 'using Genie; Genie.loadapp(); up()'
    ```
2.  The server should be running at `http://127.0.0.1:8000`.

## Running Tests
1.  Run the test suite:
    ```bash
    julia --project=. test/runtests.jl
    ```
    Or via Pkg:
    ```bash
    julia --project=. -e 'using Pkg; Pkg.test()'
    ```

## Environment Variables
*   `GENIE_ENV`: Set to `dev`, `prod`, or `test` (defaults to `dev`).
