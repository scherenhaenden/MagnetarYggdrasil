# Canonical Plan of MagnetarYggdrasil - Scala (Akka)

## Introduction
This plan captures the project's milestones, tasks, estimates, and status. Its structure must be kept intact.

## Milestones Overview Table

| Milestone ID | Name | Target Date | Description | Completion Criteria |
| :--- | :--- | :--- | :--- | :--- |
| `ms-01` | Project Initialization | 2024-05-25 | Setup project structure and governance files | All canonical files present, project builds |
| `ms-02` | Core Implementation | 2024-05-27 | Implement API and Database layers | All API endpoints functional, tests passing |

## Task Backlog Table

| Task ID | Milestone | Title | Owner | Effort (pts) | State | Notes |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| `task-101` | `ms-01` | Create canonical files | Jules | 1 | `done` | |
| `task-102` | `ms-01` | Setup Scala project structure | Jules | 2 | `in_progress` | |
| `task-201` | `ms-02` | Implement Domain Models | Jules | 2 | `planned` | |
| `task-202` | `ms-02` | Implement Repository Layer | Jules | 5 | `planned` | |
| `task-203` | `ms-02` | Implement Service Layer | Jules | 3 | `planned` | |
| `task-204` | `ms-02` | Implement API Handlers | Jules | 5 | `planned` | |
| `task-205` | `ms-02` | Write Tests | Jules | 5 | `planned` | |

## Effort Summary
-   **Total effort:** 23
-   **Completed:** 1
-   **In progress:** 2
-   **Remaining:** 20

## State Definitions
-   `planned`: Task is defined but not yet ready to start.
-   `ready`: Task is ready for work.
-   `in_progress`: Work has started.
-   `blocked`: Work is halted due to an impediment.
-   `in_review`: Work is complete and waiting for review.
-   `done`: Work is approved and merged.

## Change Management
This document must be updated whenever tasks change state or scope. Changes must be reflected in `projects/scala_akka.project.yml` and `BITACORA.md`.
