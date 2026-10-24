# Canonical Ruleset of Unison

## Introduction
These rules codify the Magnetar standard. The entire project must comply, unless a formal exception is documented in `BITACORA.md`.

## Naming Conventions
*   **Repositories:** `magnetar-<domain>-<descriptor>`.
*   **Branches:** `<type>/<short-description>` (e.g., `feature/add-login`).
*   **Tasks and Blockers:** `kebab-case` (e.g., `task-202`, `blocker-db-outage`).
*   **YAML Keys:** `lower_snake_case`.
*   **File Names:** Must mirror those in the canonical repository.

## Required Files
*   `README.md`, `PLAN.md`, `BITACORA.md`, `REQUIREMENTS.md`, `ARCHITECTURE.md`, `RULES.md`, `STATUS.md`, `TESTING.md`, `BLOCKERS.md`.
*   `BRANCHING_MODEL.md`, `WIP_GUIDELINES.md`, `CONTRIBUTING.md`.
*   `projects/<project>.project.yml`.

## Branching Conventions
*   `master`: Immutable release line.
*   `develop`: Aggregates completed features.
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
*   WIP Limit: Maximum 2 `in_progress` tasks per individual/agent.
*   Exceptions: Must be documented in `WIP_GUIDELINES.md` and `BITACORA.md`.

## Blocker Lifecycle
1.  **Discovery:** Log in `BLOCKERS.md`.
2.  **Assessment:** Update risks in `STATUS.md`.
3.  **Escalation:** Define escalation policy.
4.  **Resolution:** Document solution in `BITACORA.md`.
5.  **Retrospective:** Capture lessons learned.

## Documentation Discipline
*   `BITACORA.md`: Record every state change.
*   `STATUS.md`: Update daily or after PR merge.
*   `PLAN.md`: Source of truth for milestones.

## AI Agent Responsibilities
*   Parse project YAML.
*   Do not open PRs without confirming task state is `in_review`.
*   Document assumptions in `BITACORA.md`.

## Compliance and Enforcement
*   CI should validate required files.
*   Periodic audits will be conducted.
