# Canonical Plan of Unison

This plan captures the project's milestones, tasks, estimates, and status. Its structure must be kept intact.

## Milestones Overview Table
| Milestone ID | Name | Target Date | Description | Completion Criteria |
| :--- | :--- | :--- | :--- | :--- |
| ms-01 | Project Initiation | 2024-05-30 | Initial setup and canonical compliance | All canonical files created, YAML validated |
| ms-02 | Core Implementation | 2024-06-15 | Basic server and DB connection | Server runs, SQLite connects |
| ms-03 | Feature Complete | 2024-06-30 | Full API implementation | All endpoints working and tested |

## Task Backlog Table
| Task ID | Milestone | Title | Owner | Effort (pts) | State | Notes |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| task-101 | ms-01 | Create Canonical Files | Jules | 1 | done | Creating initial structure |
| task-102 | ms-02 | Implement HTTP Server | Jules | 5 | planned | Using Unison IO/HTTP |
| task-103 | ms-02 | Implement SQLite | Jules | 5 | planned | Using FFI or bindings |
| task-104 | ms-03 | Implement API Handlers | Jules | 8 | planned | CRUD operations |
| task-105 | ms-03 | Write Tests | Jules | 5 | planned | Unit and integration tests |

## Effort Summary
*   **Total effort:** 24
*   **Completed:** 0
*   **In progress:** 1
*   **Remaining:** 23

## State Definitions
*   `planned`: Identified but not started.
*   `ready`: Ready for work.
*   `in_progress`: Currently being worked on.
*   `blocked`: Cannot proceed.
*   `in_review`: Work completed, waiting for review.
*   `done`: Completed and verified.

## Change Management
This document must be updated whenever tasks change state or scope. Changes must be reflected in the project's YAML file and in `BITACORA.md`.
