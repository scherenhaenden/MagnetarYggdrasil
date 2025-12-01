# Canonical Project Model of PHPSymphony

## Purpose

The **PHPSymphony** project implements the MagnetarYggdrasil specification using **PHP** and the **Symfony** framework. Its purpose is to provide a benchmarkable, production-grade implementation of the standard "Users & Tasks" API, strictly adhering to the "Business Equal" criteria. This allows for a direct comparison of PHP/Symfony against other backend technologies in terms of performance, developer experience, and resource usage.

This project strictly follows the **Magnetar Canonical Project Model** for documentation, planning, and governance.

## How to Use This Repository

1.  **Clone the canonical model:** This repository structure mirrors the standard Magnetar model.
2.  **Configuration:**
    *   Copy `.env.example` to `.env`.
    *   Ensure SQLite is installed.
3.  **Governance:**
    *   Follow the WIP limits in `WIP_GUIDELINES.md`.
    *   Respect the branching model in `BRANCHING_MODEL.md`.
    *   Log all significant events in `BITACORA.md`.
4.  **Running the Project:** Refer to `README_RUN.md` for detailed build and run instructions.

## Project Contents

| File | Purpose |
| :--- | :--- |
| `PLAN.md` | Project tasks, milestones, and status. |
| `BITACORA.md` | Chronological logbook of all events. |
| `REQUIREMENTS.md` | Functional & non-functional specifications. |
| `ARCHITECTURE.md` | System structure and component design. |
| `RULES.md` | Naming conventions and workflow standards. |
| `STATUS.md` | Health summary and progress statistics. |
| `TESTING.md` | Test coverage and reporting rules. |
| `BLOCKERS.md` | Documented blockers and escalation paths. |
| `BRANCHING_MODEL.md` | Git branching strategy. |
| `WIP_GUIDELINES.md` | Work-In-Progress limits. |

## Progress Model Overview

This project tracks progress through defined states: `planned` → `ready` → `in_progress` → `in_review` → `done`.
*   **Milestones** group related tasks.
*   **Tasks** are the unit of work.
*   **State Changes** must be recorded in `BITACORA.md`.

## YAML Project Schema

The file `projects/PHPSymphony.project.yml` contains the canonical machine-readable schema for this project. It includes metadata, stakeholders, milestones, tasks, and risks. This file is the source of truth for automation tools.

## Guidance for AI Collaborators

*   **Parse** `projects/PHPSymphony.project.yml` to understand the current state.
*   **Consult** `PLAN.md` and `STATUS.md` to determine the next focus area.
*   **Adhere** to `RULES.md`, `WIP_GUIDELINES.md`, and `BRANCHING_MODEL.md`.
*   **Log** every action in `BITACORA.md` (e.g., starting a task, completing a task, identifying a blocker).

## Applying This Template

1.  Copy the repository structure (already done).
2.  Replace placeholder content with specific PHP/Symfony details.
3.  Instantiate `projects/PHPSymphony.project.yml`.
4.  Establish initial milestones and log the state in `PLAN.md`, `STATUS.md`, and `BITACORA.md`.

## Validating Canon Compliance

- [ ] All required files exist (`README.md`, `PLAN.md`, `BITACORA.md`, etc.).
- [ ] `projects/PHPSymphony.project.yml` matches the schema.
- [ ] `BITACORA.md` is updated chronologically.
- [ ] Active branches follow `BRANCHING_MODEL.md`.
- [ ] Testing and blocker processes match `TESTING.md` and `BLOCKERS.md`.
