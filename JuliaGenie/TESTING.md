# Testing Strategy for JuliaGenie

## Types of Tests
1.  **Unit Tests:**
    *   Focus on `Service` layer.
    *   Mock `Repository` layer.
    *   Verify business logic and edge cases.
2.  **Integration Tests:**
    *   Focus on `Controller` -> `Service` -> `Repository`.
    *   Use an in-memory or temporary SQLite database.
    *   Verify HTTP codes and payload structures.
3.  **End-to-End Tests:**
    *   Run against the live server.
    *   Simulate real client requests.

## Code Coverage
*   **Target:** 100%
*   **Tools:** `Test`, `Coverage`

## Bug Reporting Process
1.  Open a task in `projects/juliagenie.project.yml`.
2.  Log the bug in `BLOCKERS.md` if it stops progress.
3.  Create a fix branch `fix/...`.
4.  Add a regression test.
