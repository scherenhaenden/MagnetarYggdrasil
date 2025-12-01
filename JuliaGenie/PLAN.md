# Canonical Plan of JuliaGenie

## Introduction
This plan captures the project's milestones, tasks, estimates, and status. Its structure must be kept intact to ensure machine readability and consistency.

## Milestones Overview Table

| Milestone ID | Name | Target Date | Description | Completion Criteria |
| :--- | :--- | :--- | :--- | :--- |
| `ms-01` | Project Initialization | 2024-05-25 | Setup project structure, governance, and environment. | Directory created, all canonical files present, environment builds. |
| `ms-02` | Database & Models | 2024-05-26 | Implement SQLite connection and data models. | DB connects, schema migrates, models defined. |
| `ms-03` | API Implementation | 2024-05-27 | Implement Users and Tasks APIs. | All endpoints implemented and adhere to specs. |
| `ms-04` | Testing & Refinement | 2024-05-28 | Achieve 100% coverage and clean up. | All tests pass, coverage 100%, code reviewed. |

## Task Backlog Table

| Task ID | Milestone | Title | Owner | Effort (pts) | State | Notes |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| `task-101` | `ms-01` | Create Canonical Files | Jules | 1 | `in_progress` | Creating standard markdown files. |
| `task-102` | `ms-01` | Setup Julia Genie Scaffold | Jules | 2 | `planned` | Create standard Genie folder structure. |
| `task-103` | `ms-02` | Configure SQLite | Jules | 2 | `planned` | Setup connection and migration logic. |
| `task-104` | `ms-02` | Create User & Task Models | Jules | 3 | `planned` | Define Structs and DB mappings. |
| `task-105` | `ms-03` | Implement Users Controller | Jules | 5 | `planned` | CRUD for Users. |
| `task-106` | `ms-03` | Implement Tasks Controller | Jules | 5 | `planned` | CRUD for Tasks. |
| `task-107` | `ms-04` | Write Unit Tests | Jules | 5 | `planned` | Service layer tests. |
| `task-108` | `ms-04` | Write Integration Tests | Jules | 5 | `planned` | API endpoint tests. |

## Effort Summary
*   **Total effort:** 28 pts
*   **Completed:** 0 pts
*   **In progress:** 1 pts
*   **Remaining:** 27 pts

## State Definitions
*   `planned`: Task is defined but not yet ready to start.
*   `ready`: Task is prioritized and ready for work.
*   `in_progress`: Work has actively started.
*   `blocked`: Progress is halted by an external impediment.
*   `in_review`: Work is complete and awaiting code review/QA.
*   `done`: Work is approved and merged.

## Change Management
This document must be updated whenever tasks change state or scope. Changes must be reflected in `projects/juliagenie.project.yml` and logged in `BITACORA.md`.
