# Canonical Project Model of V (vweb)

This project is an implementation of the MagnetarYggdrasil specification using the **V** language and the **vweb** framework.

## Purpose
The purpose of this project is to provide a benchmarkable, "Business Equal" implementation of the Magnetar API to compare performance, developer experience, and resource usage against other technologies. It follows the strict Magnetar Canonical Project Model for governance and documentation.

## How to Use This Repository
1.  **Clone the canonical model:** (You are already here).
2.  **Configuration:** Check `projects/Vvweb.project.yml` for project metadata.
3.  **Documentation:** Refer to the files listed below.
4.  **Run:** See `README_RUN.md` for build and run instructions.

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
| `BRANCHING_MODEL.md` | Git branching rules. |
| `WIP_GUIDELINES.md` | Work-in-progress limits. |

## Progress Model Overview
We track progress through states: `planned` -> `in_progress` -> `in_review` -> `done`. All changes are logged in `BITACORA.md`.

## YAML Project Schema
The `projects/Vvweb.project.yml` file contains the machine-readable configuration for this project.

## Guidance for AI Collaborators
*   Parse `projects/Vvweb.project.yml`.
*   Update `BITACORA.md` for every significant action.
*   Respect `RULES.md`.
