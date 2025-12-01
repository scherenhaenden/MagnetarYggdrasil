# Canonical Ruleset of MagnetarYggdrasil - Racket

## Introduction
These rules codify the Magnetar standard. The entire project must comply, unless a formal exception is documented in `BITACORA.md`.

## Naming Conventions
*   **Repositories:** `magnetar-<domain>-<descriptor>`.
*   **Branches:** `<type>/<short-description>` (`feature`, `fix`, `chore`, `experiment`, `hotfix`).
*   **Tasks and Blockers:** `kebab-case` (e.g., `task-202`, `blocker-db-outage`).
*   **YAML Keys:** `lower_snake_case`.
*   **File Names:** Must mirror those in the canonical repository.

## Required Files
The following files **must** be included:
*   `README.md`, `PLAN.md`, `BITACORA.md`, `REQUIREMENTS.md`, `ARCHITECTURE.md`, `RULES.md`, `STATUS.md`, `TESTING.md`, `BLOCKERS.md`.
*   `BRANCHING_MODEL.md`, `WIP_GUIDELINES.md`, `CONTRIBUTING.md`.
*   `projects/Racket.project.yml`.

Any omission requires an explicit exemption logged in `BITACORA.md`.

## Branching Conventions
*   `master`: Immutable release line.
*   `develop` (optional): Aggregates completed features.
*   `feature` branches: Originate from `master` or `develop`.
*   `hotfix` branches: Start from `master`.
*   Each PR must reference tasks and include `BITACORA.md` entries.

## Allowed Task States
1.  `planned`
2.  `ready`
3.  `in_progress`
4.  `in_review`
5.  `blocked`
6.  `done`

## Work-In-Progress (WIP) Constraints
*   **WIP Limit:** Maximum 2 `in_progress` tasks per agent/person.
*   **Exceptions:** Document in `WIP_GUIDELINES.md` and `BITACORA.md`.

## Blocker Lifecycle
1.  **Discovery:** Log in `BLOCKERS.md`.
2.  **Assessment:** Update risks in `STATUS.md`, note in `BITACORA.md`.
3.  **Escalation:** After 1 business day.
4.  **Resolution:** Document in `BITACORA.md`, mark `resolved`.
5.  **Retrospective:** Capture lessons.

## Documentation Discipline
*   `BITACORA.md`: Log all events.
*   `STATUS.md`: Update daily or after merges.
*   `PLAN.md`: Source of truth for milestones/tasks.

## AI Agent Responsibilities
*   Parse project YAML.
*   Confirm task state is `in_review` before opening PRs.
*   Document assumptions in `BITACORA.md`.

## Compliance and Enforcement
*   CI checks for required files.
*   Periodic audits.
