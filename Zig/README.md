# Canonical Project Model of Zig

## Purpose
This project provides a reference implementation of the Magnetar Canonical Project Model using the Zig programming language. It serves to demonstrate how to build a standard compliant application with Zig, adhering to strict governance, documentation, and architectural standards.

## How to Use This Repository

1.  **Clone the canonical model:**
    ```bash
    git clone <repository-url>
    cd Zig
    ```
2.  **Explore the documentation:**
    Review the `README.md` and `RULES.md` to understand the governance model.
3.  **Run the application:**
    Follow the instructions in `README_RUN.md` to build and run the project.
4.  **Contribute:**
    Check `CONTRIBUTING.md` and `WIP_GUIDELINES.md` before submitting changes.
    Ensure all work is logged in `BITACORA.md`.

## Project Contents

| File | Purpose |
| :--- | :--- |
| `PLAN.md` | Project tasks & milestones. |
| `BITACORA.md` | Chronological logbook of decisions and events. |
| `REQUIREMENTS.md` | Functional & non-functional specifications. |
| `ARCHITECTURE.md` | System structure and design decisions. |
| `RULES.md` | Naming, workflow, and governance standards. |
| `STATUS.md` | Health summary & progress statistics. |
| `TESTING.md` | Test coverage & reporting strategies. |
| `BLOCKERS.md` | Impediments and escalation paths. |
| `BRANCHING_MODEL.md` | Git branching strategy. |
| `WIP_GUIDELINES.md` | Work-In-Progress limits and policies. |
| `projects/Zig.project.yml` | Machine-readable project configuration. |

## Progress Model Overview
The project tracks progress through defined states: `planned` → `in_progress` → `in_review` → `done`.
All state changes must be recorded in `BITACORA.md`. The `PLAN.md` file serves as the source of truth for task status.

## YAML Project Schema
The `projects/Zig.project.yml` file contains metadata and validation configuration for the project. It adheres to the canonical schema.

## Guidance for AI Collaborators
AI agents working on this project must:
1.  Parse `projects/Zig.project.yml` to understand context.
2.  Consult `PLAN.md` and `STATUS.md` to determine current focus.
3.  Strictly follow `RULES.md`, `WIP_GUIDELINES.md`, and `BRANCHING_MODEL.md`.
4.  Update `BITACORA.md` chronologically after every significant action.

## Validating Canon Compliance
To ensure compliance:
-   Verify all required files exist (see Project Contents).
-   Check `BITACORA.md` is up to date.
-   Ensure `projects/Zig.project.yml` is valid.
-   Confirm testing protocols in `TESTING.md` are followed.
