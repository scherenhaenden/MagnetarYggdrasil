# Testing Strategy for MagnetarYggdrasil - Crystal (Kemal)

## Types of Tests
*   **Unit Tests:** Test Services with mocked Repositories.
*   **Integration Tests:** Test Repositories with a real (in-memory or file) SQLite DB.
*   **End-to-End Tests:** Test full HTTP endpoints using `Kemal::Spec`.

## Code Coverage
Target: **100%**.

## Bug Reporting Process
1.  Open an issue in the repository.
2.  Create a `Blocker` in `BLOCKERS.md` if critical.
3.  Fix via PR with regression test.
