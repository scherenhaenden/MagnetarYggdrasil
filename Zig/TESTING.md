# Testing Strategy for Zig

## Types of Tests
-   **Unit Tests:** Verify individual functions in Services and Models.
-   **Integration Tests:** Verify Repository interactions with SQLite.
-   **End-to-End Tests:** Verify API endpoints using `std.http.Client`.

## Code Coverage
Target: **100%** coverage.

## Running Tests
Run `zig build test` to execute the full suite.

## Bug Reporting
Report bugs in `BLOCKERS.md` and create a corresponding task in `PLAN.md`.
