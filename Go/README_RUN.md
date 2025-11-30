# Run Instructions

1. **Install dependencies**:
   ```bash
   go mod tidy
   ```

2. **Run the server**:
   ```bash
   go run cmd/server/main.go
   ```

   The server will start on port 8080.
   The SQLite database `magnetar.db` will be created automatically.

3. **Run tests**:
   ```bash
   go test ./...
   ```
