# Canonical Project Model of Magnetar Swift (Vapor)

## Purpose
This project provides a reference implementation of a RESTful API using **Swift**, **Vapor 4**, and **SQLite**. It serves as part of the **MagnetarYggdrasil** benchmark, demonstrating how to build a production-ready application that strictly follows the **Magnetar Canonical Project Model** for documentation, planning, and governance.

The goal is to provide a standardized example of a layered architecture in Swift, achieving 100% test coverage and compliance with Magnetar's rigorous engineering standards.

## How to Use This Repository

### Governance and Planning
1.  **Read the Rules:** Start by reading `RULES.md` to understand the workflow and naming conventions.
2.  **Check the Plan:** Consult `PLAN.md` to see current milestones and tasks.
3.  **View Status:** Check `STATUS.md` for a high-level overview of project health.
4.  **Log Changes:** Every significant change must be recorded in `BITACORA.md`.

### Technical Usage
For detailed instructions on how to build, run, and test the code, please refer to [README_RUN.md](README_RUN.md).

Quick summary:
1.  **Run Application:** `swift run`
2.  **Run Tests:** `swift test`

## Project Contents

| File | Purpose |
| :--- | :--- |
| `PLAN.md` | Central source of truth for Project tasks & milestones. |
| `BITACORA.md` | Immutable, chronological logbook of all events. |
| `REQUIREMENTS.md` | Functional & non-functional specifications. |
| `ARCHITECTURE.md` | System structure and component design. |
| `RULES.md` | Naming conventions, workflow standards, and governance. |
| `STATUS.md` | Health summary, progress stats, and risks. |
| `TESTING.md` | Test strategy, coverage targets (100%), and reporting. |
| `BLOCKERS.md` | Registry of documented blockers and escalation paths. |
| `BRANCHING_MODEL.md` | Git branching strategy. |
| `WIP_GUIDELINES.md` | Work-In-Progress limits and policies. |
| `CONTRIBUTING.md` | Guidelines for contributors. |
| `projects/Swift.project.yml` | Machine-readable project configuration and metadata. |

## Progress Model Overview
This project tracks progress through explicit states:
-   `planned`: Task is defined but not started.
-   `in_progress`: Work has begun (limited by WIP).
-   `in_review`: Work is submitted for review (PR open).
-   `done`: Work is merged and verified.

Every state change must be manually logged in `BITACORA.md`.

## YAML Project Schema
The `projects/Swift.project.yml` file contains the canonical machine-readable schema for this project. It defines:
-   **Metadata:** Project identity and scope.
-   **Validation:** CI/CD workflow hooks.
-   **Milestones & Tasks:** Structured data mirroring `PLAN.md`.
-   **Risks:** Identified project risks.

## Guidance for AI Collaborators
AI agents working on this project must:
1.  **Parse `projects/Swift.project.yml`** to understand the context.
2.  **Read `PLAN.md` and `STATUS.md`** to determine the current focus.
3.  **Strictly adhere to `RULES.md`**, especially regarding file naming and task states.
4.  **Update `BITACORA.md`** after completing any action.
5.  **Maintain `README_RUN.md`** if build instructions change.

## Applying This Template
To apply this canonical model to a new project:
1.  Copy the entire directory structure.
2.  Replace "Swift" and "Vapor" with your target technology.
3.  Initialize `BITACORA.md` with a "Project Inception" entry.
4.  Define initial milestones in `PLAN.md` and `projects/<Project>.project.yml`.

## Validating Canon Compliance
A valid Magnetar project must have:
-   [x] All required documentation files (listed in Project Contents).
-   [x] A `projects/<Name>.project.yml` file validating against the schema.
-   [x] A `BITACORA.md` with chronological entries.
-   [x] 100% test coverage as defined in `TESTING.md`.
