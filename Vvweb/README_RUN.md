# Run Instructions for V (vweb)

## Prerequisites
*   **V**: The V programming language compiler (latest version recommended).
*   **SQLite3**: Required for the database.

## Build and Run

1.  **Install Dependencies:**
    V modules are usually managed via `v.mod` or installed globally. Since we use `vweb` and `sqlite` (built-in/standard), no extra installation is typically needed if V is installed.

2.  **Run the Application:**
    ```bash
    v run src/main.v
    ```
    The server will start on port 8080 (or as configured).

3.  **Build for Production:**
    ```bash
    v -prod src/main.v -o server
    ./server
    ```

## Testing

1.  **Run Tests:**
    ```bash
    v test .
    ```
