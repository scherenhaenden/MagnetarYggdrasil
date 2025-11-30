# Canonical Ruleset of JuliaGenie

## Introduction
This document establishes the fundamental rules and workflow standards for the **JuliaGenie** project. These rules codify the Magnetar standard, and the entire project must comply unless a formal exception is documented in `BITACORA.md`.

## Naming Conventions
*   **Repositories:** `magnetar-julia-genie` (if applicable).
*   **Branches:** `<type>/<short-description>`
    *   Types: `feature`, `fix`, `chore`, `experiment`, `hotfix`.
*   **Tasks and Blockers:** `kebab-case` (e.g., `task-101`, `blocker-db-connection`).
*   **YAML Keys:** `lower_snake_case`.
*   **File Names:** Must mirror those in the canonical repository (e.g., `UsersController.jl`).

## Required Files
The following files **must** be included:
*   `README.md`, `PLAN.md`, `BITACORA.md`, `REQUIREMENTS.md`, `ARCHITECTURE.md`, `RULES.md`, `STATUS.md`, `TESTING.md`, `BLOCKERS.md`.
*   `BRANCHING_MODEL.md`, `WIP_GUIDELINES.md`, `CONTRIBUTING.md`.
*   `projects/juliagenie.project.yml`.
*   `README_RUN.md`.

*Any omission requires an explicit exemption logged in `BITACORA.md`.*

## Branching Conventions
*   **master:** Immutable release line. Merges require successful CI and documentation updates.
*   **develop:** (Optional) Aggregates completed features before stabilization.
*   **feature branches:** Originate from `master` (or `develop`) and must be rebased before merging.
*   **hotfix branches:** Start from `master` and must trigger a `STATUS.md` update.
*   **Pull Requests:** Must reference tasks and include `BITACORA.md` entries.

## Allowed Task States
1.  `planned`
2.  `ready`
3.  `in_progress`
4.  `in_review`
5.  `blocked`
6.  `done`

*Transitions example:* `ready` → `in_progress` when work begins.

## Work-In-Progress (WIP) Constraints
*   **WIP Limit:** Maximum **2** `in_progress` tasks per individual or AI agent.
*   **Exceptions:** Must be approved and documented in `WIP_GUIDELINES.md` and `BITACORA.md`.

## Blocker Lifecycle
1.  **Discovery:** Log in `BLOCKERS.md`.
2.  **Assessment:** Update `STATUS.md` and `BITACORA.md`.
3.  **Escalation:** Escalate if not resolved within **1 business day**.
4.  **Resolution:** Document solution in `BITACORA.md` and mark `resolved`.
5.  **Retrospective:** Capture lessons learned.

## Documentation Discipline
*   **BITACORA.md:** Chronologically record every state change, decision, or exception.
*   **STATUS.md:** Update at least daily or after PR merges.
*   **PLAN.md:** Source of truth for milestones and assignments.

## AI Agent Responsibilities
*   Parse `projects/juliagenie.project.yml`.
*   Do not open PRs without confirming task state is `in_review`.
*   Document assumptions in `BITACORA.md`.

## Compliance and Enforcement
*   CI checks should validate presence of required files.
*   Periodic audits will be conducted.
