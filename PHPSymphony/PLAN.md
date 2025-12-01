# Canonical Plan of PHPSymphony

This plan captures the project's milestones, tasks, estimates, and status. Its structure must be kept intact.

## Milestones Overview Table

| Milestone ID | Name | Target Date | Description | Completion Criteria |
| :--- | :--- | :--- | :--- | :--- |
| `ms-01` | Project Setup & Scaffold | 2024-05-25 | Initialize project structure, dependencies, and documentation. | Directory structure exists, `composer.json` created, documentation files created. |
| `ms-02` | Database & Entities | 2024-05-26 | Implement Doctrine Entities and SQLite configuration. | User and Task entities created, database connection configured. |
| `ms-03` | Core API Implementation | 2024-05-27 | Implement Controllers, Services, and Repositories for Users and Tasks. | All endpoints defined in `REQUIREMENTS.md` are implemented. |
| `ms-04` | Testing & Refinement | 2024-05-28 | Achieve 100% test coverage and ensure "Business Equal" compliance. | Unit, Integration, and E2E tests pass. |

## Task Backlog Table

| Task ID | Milestone | Title | Owner | Effort (pts) | State | Notes |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| `task-101` | `ms-01` | Create Directory Structure & Documentation | Jules | 1 | `in_progress` | Creating canonical files. |
| `task-102` | `ms-01` | Initialize Symfony Project Files | Jules | 2 | `planned` | Manual creation of `composer.json`, `public/index.php`, etc. |
| `task-201` | `ms-02` | Configure SQLite & Doctrine | Jules | 2 | `planned` | `config/packages/doctrine.yaml` and `.env`. |
| `task-202` | `ms-02` | Implement User Entity | Jules | 2 | `planned` | |
| `task-203` | `ms-02` | Implement Task Entity | Jules | 2 | `planned` | |
| `task-301` | `ms-03` | Implement User Repository | Jules | 2 | `planned` | |
| `task-302` | `ms-03` | Implement Task Repository | Jules | 2 | `planned` | |
| `task-303` | `ms-03` | Implement User Service | Jules | 3 | `planned` | Business logic for Users. |
| `task-304` | `ms-03` | Implement Task Service | Jules | 3 | `planned` | Business logic for Tasks. |
| `task-305` | `ms-03` | Implement User Controller | Jules | 3 | `planned` | JSON responses. |
| `task-306` | `ms-03` | Implement Task Controller | Jules | 3 | `planned` | JSON responses. |
| `task-307` | `ms-03` | Implement System Controller | Jules | 1 | `planned` | `/health` endpoint. |
| `task-401` | `ms-04` | Write Unit Tests | Jules | 5 | `planned` | Mock repositories. |
| `task-402` | `ms-04` | Write Integration Tests | Jules | 5 | `planned` | Test Service layer with DB. |
| `task-403` | `ms-04` | Write E2E Tests | Jules | 5 | `planned` | Test API endpoints. |

## Effort Summary

*   **Total effort:** 41 pts
*   **Completed:** 0 pts
*   **In progress:** 1 pts
*   **Remaining:** 40 pts

## State Definitions

*   `planned`: Identified but not yet started.
*   `ready`: Ready to be picked up.
*   `in_progress`: Currently being worked on.
*   `blocked`: Cannot proceed due to an impediment.
*   `in_review`: Completed and awaiting code/documentation review.
*   `done`: Completed and merged.

## Change Management

This document must be updated whenever tasks change state or scope. Changes must be reflected in `projects/PHPSymphony.project.yml` and `BITACORA.md`.
