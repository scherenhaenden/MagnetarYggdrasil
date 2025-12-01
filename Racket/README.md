# Canonical Project Model of MagnetarYggdrasil - Racket

## Purpose
This project is a Racket implementation of the Magnetar Canonical Project. It exists to demonstrate the Magnetar standard for documentation, planning, and governance within the context of a Racket web application.

## How to Use This Repository
1.  **Clone the canonical model.**
2.  **Copy and fill out the `projects/Racket.project.yml` file.**
3.  **Replicate the required documentation set.**
4.  **Follow the WIP, branching, and blocker rules.**
5.  **Consult the example project to resolve questions.**

## Project Contents
| File | Purpose |
| :--- | :--- |
| `PLAN.md` | Project tasks & milestones. |
| `BITACORA.md` | Chronological logbook. |
| `REQUIREMENTS.md` | Functional & non-functional specs. |
| `ARCHITECTURE.md` | System/module structure. |
| `RULES.md` | Naming & workflow standards. |
| `STATUS.md` | Health summary & progress stats. |
| `TESTING.md` | Test coverage & reporting rules. |
| `BLOCKERS.md` | Documented blockers & escalation paths. |
| `BRANCHING_MODEL.md` | Branching strategy. |
| `WIP_GUIDELINES.md` | Work-In-Progress limits. |
| `CONTRIBUTING.md` | Contribution guidelines. |

## Progress Model Overview
The project tracks progress through milestones and tasks with states: `planned` → `in_progress` → `in_review` → `done`. Every state change is recorded in `BITACORA.md`.

## YAML Project Schema
The `projects/Racket.project.yml` file contains the canonical machine-readable schema with metadata, stakeholders, milestones, tasks, and risks.

## Guidance for AI Collaborators
*   Parse the project YAML file.
*   Use `PLAN.md` and `STATUS.md` to determine focus.
*   Respect `RULES.md`, `WIP_GUIDELINES.md`, and `BRANCHING_MODEL.md`.
*   Update `BITACORA.md` after completing any work.

## Applying This Template
1.  Copy the repository structure.
2.  Replace placeholder content with project-specific details.
3.  Instantiate and validate a project YAML file.
4.  Establish initial milestones and log the initial state in `PLAN.md`, `STATUS.md`, and `BITACORA.md`.

## Validating Canon Compliance
*   [ ] All required files exist.
*   [ ] The project YAML matches the schema.
*   [ ] `BITACORA.md` is updated chronologically.
*   [ ] Active branches follow the `BRANCHING_MODEL.md` rules.
*   [ ] Testing commitments and blocker processes match `TESTING.md` and `BLOCKERS.md`.
