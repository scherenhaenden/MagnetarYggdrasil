# Canonical Plan of F# (.NET AOT)

## Introduction
This plan captures the project's milestones, tasks, estimates, and status. Its structure must be kept intact.

## Milestones Overview Table

| Milestone ID | Name | Target Date | Description | Completion Criteria |
| :--- | :--- | :--- | :--- | :--- |
| `ms-01` | Project Initialization | 2025-11-30 | Setup project structure and documentation. | All canonical docs created, project compiles. |
| `ms-02` | Core Implementation | 2025-12-01 | Implement Domain, Repository, Service. | All layers implemented, unit tests pass. |
| `ms-03` | API & AOT | 2025-12-02 | Implement Handlers, Web Server, AOT build. | API endpoints work, AOT compilation succeeds. |

## Task Backlog Table

| Task ID | Milestone | Title | Owner | Effort (pts) | Weight (%) | State | Notes |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| `task-001` | `ms-01` | Create Directory & Docs | Jules | 1 | 5% | `done` | |
| `task-002` | `ms-02` | Implement Domain Models | Jules | 1 | 5% | `done` | |
| `task-003` | `ms-02` | Implement Repository (SQLite) | Jules | 5 | 25% | `done` | Raw ADO.NET |
| `task-004` | `ms-02` | Implement Service Layer | Jules | 3 | 15% | `done` | |
| `task-005` | `ms-03` | Implement HTTP Handlers | Jules | 3 | 15% | `done` | Minimal APIs |
| `task-006` | `ms-03` | Configure AOT & Entry Point | Jules | 2 | 10% | `done` | |
| `task-007` | `ms-02` | Write Unit Tests | Jules | 3 | 15% | `done` | Moq |
| `task-008` | `ms-03` | Write Integration Tests | Jules | 2 | 10% | `done` | TestHost |
| `task-009` | `ms-03` | Create Dockerfile | Jules | 1 | 5% | `done` | Multi-stage build |

## Effort Summary
*   **Total effort:** 21 pts
*   **Completed:** 21 pts
*   **In progress:** 0 pts
*   **Remaining:** 0 pts

## State Definitions
*   `planned`: Identified but not started.
*   `ready`: Ready for implementation.
*   `in_progress`: Currently being worked on.
*   `blocked`: Waiting on external factor.
*   `in_review`: Waiting for code review/merge.
*   `done`: Completed and merged.

## Change Management
Update this document whenever tasks change state or scope. Reflect changes in `projects/fsharpaot.project.yml` and `BITACORA.md`.
