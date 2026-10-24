# Canonical Project Model of Unison

## Purpose
This project provides a Unison implementation of the Magnetar standard application. It serves to demonstrate how to structure a Unison project following strict governance, documentation, and architectural patterns, ensuring consistency across the Magnetar ecosystem.

## How to Use This Repository
1.  **Clone the canonical model:** Clone this repository to your local machine.
2.  **Copy and fill out the project YAML:** Locate `projects/Unison.project.yml` and update it if necessary.
3.  **Replicate the required documentation set:** Ensure all files listed in the "Project Contents" section exist and are up to date.
4.  **Follow the WIP, branching, and blocker rules:** Adhere to `RULES.md`, `WIP_GUIDELINES.md`, and `BRANCHING_MODEL.md`.
5.  **Consult the example project:** Use this implementation as a reference for other Magnetar projects.

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
| `BRANCHING_MODEL.md` | Governance reference for branching. |
| `WIP_GUIDELINES.md` | Governance reference for WIP limits. |

## Progress Model Overview
The project tracks progress through milestones and tasks with states: `planned` → `in_progress` → `in_review` → `done`. Every state change is strictly recorded in `BITACORA.md`.

## YAML Project Schema
The `projects/Unison.project.yml` file contains the canonical machine-readable schema with metadata, stakeholders, milestones, tasks, and risks. AI agents must parse this file to understand the project state.

## Guidance for AI Collaborators
*   Parse `projects/Unison.project.yml`.
*   Use `PLAN.md` and `STATUS.md` to determine focus.
*   Respect `RULES.md`, `WIP_GUIDELINES.md`, and `BRANCHING_MODEL.md`.
*   Update `BITACORA.md` after completing any work.

## Applying This Template
1.  Copy the repository structure.
2.  Replace placeholder content with project-specific details.
3.  Instantiate and validate a project YAML file.
4.  Establish initial milestones and log the initial state.

## Validating Canon Compliance
*   [ ] All required files exist.
*   [ ] The project YAML matches the schema.
*   [ ] `BITACORA.md` is updated chronologically.
*   [ ] Active branches follow `BRANCHING_MODEL.md` rules.
*   [ ] Testing commitments match `TESTING.md`.
