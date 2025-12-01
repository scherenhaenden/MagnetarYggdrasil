# Testing Strategy for V (vweb)

## Types of Tests
1.  **Unit Tests:** Test Service layer logic (mocking repository if possible, or using in-memory DB).
2.  **Integration Tests:** Test HTTP endpoints against a real (test) SQLite database.
3.  **End-to-End Tests:** `curl` based script to verify running server.

## Code Coverage
Target: 100%
