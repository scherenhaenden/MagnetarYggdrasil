# Canonical Project Model of AdaSPARK

## Purpose
This project is an implementation of the Magnetar application using Ada/SPARK. It follows the Magnetar standard for documentation, planning, and governance.

## How to Use This Repository
1.  **Clone the canonical model.**
2.  **Copy and fill out the `projects/_template.project.yml` file.**
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
| `BRANCHING_MODEL.md` | Governance references. |
| `WIP_GUIDELINES.md` | Governance references. |

## Progress Model Overview
The project tracks progress through milestones, tasks, and state transitions (`planned` → `in_progress` → `in_review` → `done`). Every state change is recorded in `BITACORA.md`.

## YAML Project Schema
The `projects/adaspark.project.yml` file contains the canonical machine-readable schema with metadata, stakeholders, milestones, tasks, and risks.

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
*   All required files exist.
*   The project YAML matches the schema.
*   `BITACORA.md` is updated chronologically.
*   Active branches follow the `BRANCHING_MODEL.md` rules.
*   Testing commitments and blocker processes match `TESTING.md` and `BLOCKERS.md`.
