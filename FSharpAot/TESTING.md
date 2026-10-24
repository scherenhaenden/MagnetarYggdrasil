# Testing Strategy for F# (.NET AOT)

## Types of Tests
1.  **Unit Tests:**
    *   Target: Service Layer.
    *   Technique: Mock Repository using `Moq`.
    *   Framework: `xUnit`.
2.  **Integration Tests:**
    *   Target: Full API (Handlers -> Service -> Repository).
    *   Technique: `TestServer` (InMemory) with SQLite (File or InMemory).
    *   Framework: `xUnit`, `Microsoft.AspNetCore.TestHost`.

## Code Coverage
*   **Target:** 100%

## Bug Reporting Process
1.  Open issue in tracker.
2.  Create test case reproducing the bug.
3.  Fix bug.
4.  Verify fix.
