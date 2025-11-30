# Testing Strategy for PHPSymphony

## Types of Tests

1.  **Unit Tests (`tests/Unit/`)**
    *   **Scope:** Service Layer.
    *   **Strategy:** Mock Repositories. Test business logic, validations, and error handling.
    *   **Tool:** PHPUnit.

2.  **Integration Tests (`tests/Integration/`)**
    *   **Scope:** Repository Layer and Database interactions.
    *   **Strategy:** Use a real SQLite database (in-memory or temporary file). Test CRUD operations and constraints.
    *   **Tool:** PHPUnit, Symfony KernelTestCase.

3.  **End-to-End Tests (`tests/E2E/`)**
    *   **Scope:** Full API (Controller -> Service -> Repository -> DB).
    *   **Strategy:** Make real HTTP requests to the application.
    *   **Tool:** PHPUnit, Symfony WebTestCase.

## Code Coverage
*   **Target:** 100% Line Coverage.
*   **Reporting:** Generated via PHPUnit (Clover/HTML).

## Bug Reporting Process
1.  Open an issue/blocker.
2.  Create a failing test case that reproduces the bug.
3.  Fix the bug.
4.  Verify the test passes.
