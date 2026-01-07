# Canonical Ruleset of Zig

## Introduction
These rules codify the Magnetar standard for the Zig project. Compliance is mandatory unless a formal exception is documented in `BITACORA.md`.

## Naming Conventions
-   **Branches:** `<type>/<short-description>` (e.g., `feature/user-auth`, `fix/db-connection`).
-   **Tasks:** `kebab-case` (e.g., `task-101`, `implement-api`).
-   **Files:** Must follow the structure defined in the canonical repository.

## Required Files
The following files **must** exist:
-   `README.md`, `PLAN.md`, `BITACORA.md`, `REQUIREMENTS.md`, `ARCHITECTURE.md`
-   `RULES.md`, `STATUS.md`, `TESTING.md`, `BLOCKERS.md`
-   `BRANCHING_MODEL.md`, `WIP_GUIDELINES.md`, `CONTRIBUTING.md`
-   `projects/Zig.project.yml`

## Branching Conventions
-   `master`: Immutable release line.
-   `feature/*`: Feature branches off `master` (or `develop`).
-   PRs must reference tasks and include `BITACORA.md` updates.

## Allowed Task States
1.  `planned`
2.  `ready`
3.  `in_progress`
4.  `in_review`
5.  `blocked`
6.  `done`

## Work-In-Progress (WIP) Constraints
-   **Limit:** Maximum 2 `in_progress` tasks per contributor.
-   **Exceptions:** Must be approved and logged in `BITACORA.md`.

## Blocker Lifecycle
1.  **Discovery:** Log in `BLOCKERS.md`.
2.  **Assessment:** Update `STATUS.md` risks.
3.  **Resolution:** Fix and document in `BITACORA.md`.

## Documentation Discipline
-   **BITACORA.md:** Record every state change/decision.
-   **STATUS.md:** Update daily or on PR merge.
-   **PLAN.md:** Source of truth for tasks.

## AI Agent Responsibilities
-   Read project configuration first.
-   Do not open PRs unless the task is `in_review`.
-   Document everything.
