# Canonical Ruleset of Magnetar Swift

## Introduction
These rules codify the **Magnetar Canonical Project Model** standard. The entire Swift project must comply with these rules. Any deviation requires a formal exception documented in `BITACORA.md`.

## Naming Conventions
-   **Repositories:** `magnetar-swift-vapor` (or similar).
-   **Branches:** `<type>/<short-description>`
    -   Types: `feature`, `fix`, `chore`, `experiment`, `hotfix`.
    -   Example: `feature/user-auth`, `fix/db-connection`.
-   **Tasks:** `kebab-case` (e.g., `task-101`, `task-init-db`).
-   **Blockers:** `kebab-case` (e.g., `blocker-ci-failure`).
-   **YAML Keys:** `lower_snake_case`.

## Required Files
The following files **must** be present in the `Swift/` directory:
-   `README.md`, `PLAN.md`, `BITACORA.md`, `REQUIREMENTS.md`, `ARCHITECTURE.md`, `RULES.md`, `STATUS.md`, `TESTING.md`, `BLOCKERS.md`.
-   `BRANCHING_MODEL.md`, `WIP_GUIDELINES.md`, `CONTRIBUTING.md`.
-   `projects/Swift.project.yml`.
-   `README_RUN.md` (Technical running instructions).

Any omission must be logged as an exception in `BITACORA.md`.

## Branching Conventions
-   **`master`:** Immutable release line. Merges require passing tests and updated documentation.
-   **`feature`:** Created from `master`. Must be rebased before merging.
-   **`hotfix`:** Created from `master`. Requires `STATUS.md` update upon completion.
-   **Pull Requests:** Must reference related task IDs and include `BITACORA.md` updates.

## Allowed Task States
1.  `planned`: Defined in backlog.
2.  `ready`: Prioritized for immediate work.
3.  `in_progress`: Currently being worked on.
4.  `in_review`: PR open, waiting for merge.
5.  `blocked`: Cannot proceed due to external factor.
6.  `done`: Merged and verified.

## Work-In-Progress (WIP) Constraints
-   **Limit:** Maximum **2** `in_progress` tasks per contributor (human or AI).
-   **Exceptions:** Must be approved and logged in `BITACORA.md`.

## Blocker Lifecycle
1.  **Discovery:** Log in `BLOCKERS.md`.
2.  **Assessment:** Update risks in `STATUS.md`.
3.  **Resolution:** document solution in `BITACORA.md` and mark as `resolved` in `BLOCKERS.md`.

## Documentation Discipline
-   **`BITACORA.md`:** Must record **every** state change, decision, and discovery.
-   **`STATUS.md`:** Update daily or after significant merges.
-   **`PLAN.md`:** Source of truth for task status.

## AI Agent Responsibilities
-   Always parse `projects/Swift.project.yml` first.
-   Verify task state is `ready` before moving to `in_progress`.
-   Document all assumptions in `BITACORA.md`.

## Compliance
-   CI should validate file presence.
-   Periodic audits will check for `BITACORA.md` consistency.
