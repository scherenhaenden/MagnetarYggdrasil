# Canonical Ruleset of F# (.NET AOT)

## Introduction
These rules codify the Magnetar standard for the F# (.NET AOT) implementation. The entire project must comply unless a formal exception is documented in `BITACORA.md`.

## Naming Conventions
*   **Repositories:** `magnetar-fsharp-aot` (Internal context).
*   **Branches:** `<type>/<short-description>` (e.g., `feature/user-api`, `fix/db-connection`). Types: `feature`, `fix`, `chore`, `experiment`, `hotfix`.
*   **Tasks and Blockers:** `kebab-case` (e.g., `task-101`, `blocker-db-lock`).
*   **YAML Keys:** `lower_snake_case`.
*   **File Names:** Must mirror those in the canonical repository.

## Required Files
The following files **must** be included:
*   `README.md`, `PLAN.md`, `BITACORA.md`, `REQUIREMENTS.md`, `ARCHITECTURE.md`, `RULES.md`, `STATUS.md`, `TESTING.md`, `BLOCKERS.md`.
*   `BRANCHING_MODEL.md`, `WIP_GUIDELINES.md`, `CONTRIBUTING.md`.
*   `projects/fsharpaot.project.yml`.
*   Any omission requires an explicit exemption logged in `BITACORA.md`.

## Branching Conventions
*   `master`: Immutable release line.
*   `develop`: Aggregates completed features.
*   `feature`: Originate from `develop`, rebase before merge.
*   `hotfix`: Start from `master`.
*   PRs must reference tasks and include `BITACORA.md` entries.

## Allowed Task States
1.  `planned`
2.  `ready`
3.  `in_progress`
4.  `in_review`
5.  `blocked`
6.  `done`

## Work-In-Progress (WIP) Constraints
*   **WIP Limit:** Maximum 2 `in_progress` tasks per agent/person.
*   **Exceptions:** Document in `BITACORA.md`.

## Blocker Lifecycle
1.  **Discovery:** Log in `BLOCKERS.md`.
2.  **Assessment:** Update `STATUS.md`, note in `BITACORA.md`.
3.  **Escalation:** After 1 business day.
4.  **Resolution:** Document in `BITACORA.md`, set to `resolved`.
5.  **Retrospective:** Capture lessons.

## Documentation Discipline
*   `BITACORA.md`: Record every state change.
*   `STATUS.md`: Update daily or after PR merge.
*   `PLAN.md`: Source of truth for milestones/tasks.

## AI Agent Responsibilities
*   Parse project YAML.
*   Do not open PRs without `in_review` state.
*   Document assumptions in `BITACORA.md`.

## Compliance and Enforcement
*   CI should validate required files.
*   Periodic audits will be conducted.
