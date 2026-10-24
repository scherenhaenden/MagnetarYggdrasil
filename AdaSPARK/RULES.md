# Canonical Ruleset of AdaSPARK

## Introduction
These rules codify the Magnetar standard and the entire project must comply, unless a formal exception is documented in `BITACORA.md`.

## Naming Conventions
*   **Repositories:** `magnetar-<domain>-<descriptor>`
*   **Branches:** `<type>/<short-description>`
*   **Tasks and Blockers:** `kebab-case`
*   **YAML Keys:** `lower_snake_case`

## Required Files
*   `README.md`, `PLAN.md`, `BITACORA.md`, `REQUIREMENTS.md`, `ARCHITECTURE.md`, `RULES.md`, `STATUS.md`, `TESTING.md`, `BLOCKERS.md`.
*   `BRANCHING_MODEL.md`, `WIP_GUIDELINES.md`, `CONTRIBUTING.md`.
*   `projects/<project>.project.yml`.

## Branching Conventions
*   `master`: Immutable release line.
*   `develop`: Aggregates completed features.
*   `feature` branches: Originate from `master` or `develop`.

## Allowed Task States
1.  `planned`
2.  `ready`
3.  `in_progress`
4.  `in_review`
5.  `blocked`
6.  `done`

## Work-In-Progress (WIP) Constraints
*   WIP Limit: 2 tasks per agent.

## Blocker Lifecycle
1.  **Discovery**
2.  **Assessment**
3.  **Escalation**
4.  **Resolution**
5.  **Retrospective**

## Documentation Discipline
*   `BITACORA.md`: Record every state change.
*   `STATUS.md`: Update daily.
*   `PLAN.md`: Source of truth.

## AI Agent Responsibilities
*   Parse YAML.
*   Update `BITACORA.md`.
*   Respect rules.

## Compliance and Enforcement
*   CI checks required files.
