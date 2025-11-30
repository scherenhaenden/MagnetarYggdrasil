# Canonical Ruleset of Lua (OpenResty)

## Introduction
These rules codify the Magnetar standard for the Lua OpenResty implementation. The entire project must comply unless a formal exception is documented in `BITACORA.md`.

## Naming Conventions
*   **Repositories:** `magnetar-lua-openresty` (Conceptually, part of monorepo).
*   **Branches:** `<type>/<short-description>` (e.g., `feature/user-api`, `fix/db-connection`).
*   **Tasks:** `kebab-case` (e.g., `task-001`, `task-db-setup`).
*   **Files:** `snake_case` or `kebab-case` as appropriate for Lua/OpenResty standards.

## Required Files
*   `README.md`, `PLAN.md`, `BITACORA.md`, `REQUIREMENTS.md`, `ARCHITECTURE.md`, `RULES.md`, `STATUS.md`, `TESTING.md`, `BLOCKERS.md`.
*   `BRANCHING_MODEL.md`, `WIP_GUIDELINES.md`.
*   `projects/lua-openresty.project.yml`.

## Branching Conventions
*   `master`: Immutable release line.
*   `feature` branches: Originate from `master`.
*   Pull Requests must reference tasks and include `BITACORA.md` entries.

## Allowed Task States
1.  `planned`
2.  `ready`
3.  `in_progress`
4.  `in_review`
5.  `blocked`
6.  `done`

## Work-In-Progress (WIP) Constraints
*   **WIP Limit:** Maximum 2 `in_progress` tasks per agent.
*   Exceptions must be logged in `BITACORA.md`.

## Blocker Lifecycle
1.  **Discovery:** Log in `BLOCKERS.md`.
2.  **Assessment:** Update `STATUS.md`.
3.  **Resolution:** Document in `BITACORA.md`.

## Documentation Discipline
*   `BITACORA.md`: Record every state change.
*   `STATUS.md`: Update daily or after merges.
*   `PLAN.md`: Source of truth for tasks.

## AI Agent Responsibilities
*   Parse YAML before acting.
*   Document assumptions in `BITACORA.md`.
