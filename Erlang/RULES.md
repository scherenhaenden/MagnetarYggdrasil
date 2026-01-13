# Canonical Ruleset of Erlang Project

## Introduction
These rules codify the Magnetar standard for the Erlang implementation.

## Naming Conventions
*   **Repositories:** `magnetar-<domain>-<descriptor>`
*   **Branches:** `<type>/<short-description>`
*   **Tasks:** `kebab-case`
*   **File Names:** Mirror canonical repository.

## Required Files
*   `README.md`, `PLAN.md`, `BITACORA.md`, `REQUIREMENTS.md`, `ARCHITECTURE.md`, `RULES.md`, `STATUS.md`, `TESTING.md`, `BLOCKERS.md`.
*   `BRANCHING_MODEL.md`, `WIP_GUIDELINES.md`, `CONTRIBUTING.md`.

## Branching Conventions
*   `master`: Immutable release line.
*   `feature` branches: Rebase before merging.

## Allowed Task States
1.  `planned`
2.  `ready`
3.  `in_progress`
4.  `in_review`
5.  `blocked`
6.  `done`

## Work-In-Progress (WIP) Constraints
*   Max 2 tasks in progress per agent.

## Blocker Lifecycle
1.  Discovery
2.  Assessment
3.  Escalation
4.  Resolution
5.  Retrospective

## Documentation Discipline
*   Update `BITACORA.md` for every state change.
*   Update `STATUS.md` daily.
*   `projects/Erlang.project.yml`.
