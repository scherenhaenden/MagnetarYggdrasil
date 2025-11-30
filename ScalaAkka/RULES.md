# Canonical Ruleset of MagnetarYggdrasil - Scala (Akka)

## Introduction
These rules codify the Magnetar standard. The entire project must comply, unless a formal exception is documented in `BITACORA.md`.

## Naming Conventions
-   **Repositories:** `magnetar-<domain>-<descriptor>`
-   **Branches:** `<type>/<short-description>` (types: `feature`, `fix`, `chore`, `experiment`, `hotfix`)
-   **Tasks and Blockers:** `kebab-case` (e.g., `task-202`, `blocker-db-outage`)
-   **YAML Keys:** `lower_snake_case`
-   **File Names:** Must mirror those in the canonical repository.

## Required Files
Every Magnetar project **must** include:
-   `README.md`, `PLAN.md`, `BITACORA.md`, `REQUIREMENTS.md`, `ARCHITECTURE.md`, `RULES.md`, `STATUS.md`, `TESTING.md`, `BLOCKERS.md`
-   `BRANCHING_MODEL.md`, `WIP_GUIDELINES.md`, `CONTRIBUTING.md`
-   `projects/<project>.project.yml`

Any omission requires an explicit exemption logged in `BITACORA.md`.

## Branching Conventions
-   `master`: Immutable release line; merges require successful CI and documentation updates.
-   `develop` (optional): Aggregates completed features before stabilization.
-   `feature` branches: Originate from `master` or `develop` and must be rebased before merging.
-   `hotfix` branches: Start from `master` and must trigger a `STATUS.md` update upon completion.
-   Each PR must reference the tasks it affects and include `BITACORA.md` entries.

## Allowed Task States
1.  `planned`
2.  `ready`
3.  `in_progress`
4.  `in_review`
5.  `blocked`
6.  `done`

Transitions: `ready` → `in_progress` → `in_review` → `done`.

## Work-In-Progress (WIP) Constraints
-   **WIP Limit:** Maximum 2 `in_progress` tasks per individual or AI agent.
-   **Exceptions:** Must be approved and documented in `WIP_GUIDELINES.md` and `BITACORA.md`.

## Blocker Lifecycle
1.  **Discovery:** Log in `BLOCKERS.md`.
2.  **Assessment:** Update risks in `STATUS.md`, note mitigation in `BITACORA.md`.
3.  **Escalation:** Escalate if not resolved within 1 business day.
4.  **Resolution:** Document solution in `BITACORA.md`, set status to `resolved`.
5.  **Retrospective:** Capture lessons learned.

## Documentation Discipline
-   `BITACORA.md`: Chronologically record every state change, decision, or exception.
-   `STATUS.md`: Update at least daily or after each PR merge.
-   `PLAN.md`: Source of truth for milestones and task assignments.

## AI Agent Responsibilities
-   Parse the project YAML file before acting.
-   Do not open PRs without confirming the task state is `in_review`.
-   Document assumptions in `BITACORA.md`.

## Compliance and Enforcement
-   CI should validate the presence and structure of required files.
-   Periodic audits will be conducted.
