# Testing Strategy

- **Unit Tests**: Test Service logic, mocking Repository functions.
- **Integration Tests**: Test Handlers with a real (in-memory or file) SQLite database, covering the full stack.
- **End-to-End**: External script to verify the running server.
