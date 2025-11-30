# Canonical Project Model of JuliaGenie

## Purpose
This project provides a reference implementation of the MagnetarYggdrasil specification using the **Julia** programming language and the **Genie** framework. It serves to benchmark and compare Julia's performance, developer experience, and ecosystem maturity against other backend technologies in a controlled, "Business Equal" environment. This project strictly follows the Magnetar standard for documentation, planning, and governance to ensure consistency and reliability.

## How to Use This Repository

1.  **Clone the canonical model:**
    ```bash
    git clone <repository-url>
    cd JuliaGenie
    ```
2.  **Initialize the Project:**
    Follow the instructions in `README_RUN.md` to set up the Julia environment and install dependencies.
3.  **Consult Documentation:**
    Read `PLAN.md` to understand current tasks and `RULES.md` for workflow standards.
4.  **Verify Compliance:**
    Ensure any changes adhere to `ARCHITECTURE.md` and `TESTING.md`.

## Project Contents

| File | Purpose |
| :--- | :--- |
| `PLAN.md` | Project tasks, milestones, and estimates. |
| `BITACORA.md` | Chronological logbook of all events and decisions. |
| `REQUIREMENTS.md` | Functional and non-functional specifications. |
| `ARCHITECTURE.md` | System structure, layers, and components. |
| `RULES.md` | Naming conventions, workflow rules, and standards. |
| `STATUS.md` | Health summary, progress stats, and risks. |
| `TESTING.md` | Test strategy, coverage targets, and reporting. |
| `BLOCKERS.md` | Registry of impediments and escalation paths. |
| `BRANCHING_MODEL.md` | Git branching strategy (Governance). |
| `WIP_GUIDELINES.md` | Work-In-Progress limits and policies (Governance). |

## Progress Model Overview
This project tracks progress through defined states: `planned` → `ready` → `in_progress` → `in_review` → `done`.
*   **Planning:** Milestones and tasks are defined in `projects/juliagenie.project.yml` and reflected in `PLAN.md`.
*   **Execution:** Work is tracked in `STATUS.md`.
*   **Logging:** Every state change, decision, or blocker is effectively recorded in `BITACORA.md`.

## YAML Project Schema
The `projects/juliagenie.project.yml` file contains the canonical machine-readable schema for this project. It defines metadata, stakeholders, milestones, tasks, and risks, serving as the single source of truth for automation tools and AI agents.

## Guidance for AI Collaborators
AI agents working on this project must:
1.  **Parse** `projects/juliagenie.project.yml` to understand the current context.
2.  **Consult** `PLAN.md` and `STATUS.md` to identify focus areas.
3.  **Respect** `RULES.md`, `WIP_GUIDELINES.md`, and `BRANCHING_MODEL.md`.
4.  **Log** all actions and state changes in `BITACORA.md`.

## Applying This Template
To replicate this model:
1.  Copy the repository structure.
2.  Replace placeholder content with project-specific details.
3.  Instantiate `projects/juliagenie.project.yml`.
4.  Establish initial milestones and log the initial state.

## Validating Canon Compliance
*   [ ] All required files exist.
*   [ ] `projects/juliagenie.project.yml` matches the schema.
*   [ ] `BITACORA.md` is updated chronologically.
*   [ ] Active branches follow `BRANCHING_MODEL.md`.
*   [ ] Testing commitments match `TESTING.md`.
