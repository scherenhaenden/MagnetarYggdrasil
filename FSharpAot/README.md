# Canonical Project Model of F# (.NET AOT)

## Purpose
This project is an implementation of the MagnetarYggdrasil specification using F# and .NET 10 Native AOT. It demonstrates the performance and developer experience of functional programming on the .NET platform for backend web API development, adhering to the "Business Equal" criteria. It follows the Magnetar standard for documentation, planning, and governance to ensure consistency with other implementations.

## How to Use This Repository

1.  **Clone the canonical model:**
    ```bash
    git clone <repo_url>
    cd magnetar-yggdrasil/FSharpAot
    ```

2.  **Copy and fill out the project file:**
    Copy `projects/_template.project.yml` to `projects/fsharpaot.project.yml` and fill in the details.

3.  **Replicate the required documentation set:**
    Ensure all files listed below exist and are up to date.

4.  **Follow the rules:**
    Respect the `RULES.md`, `WIP_GUIDELINES.md`, and `BRANCHING_MODEL.md`.

5.  **Consult the example project:**
    Check other implementations like `Rust` or `Go` for reference if needed.

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
| `BRANCHING_MODEL.md` | Git branching strategy. |
| `WIP_GUIDELINES.md` | Work-In-Progress limits. |

## Progress Model Overview

The project tracks progress through milestones and task states: `planned` → `ready` → `in_progress` → `in_review` → `done`.
Every state change is recorded in `BITACORA.md`.

## YAML Project Schema

The `projects/_template.project.yml` file contains the canonical machine-readable schema with metadata, stakeholders, milestones, tasks, and risks. AI agents must parse this file.

## Guidance for AI Collaborators

*   Parse `projects/fsharpaot.project.yml`.
*   Use `PLAN.md` and `STATUS.md` to determine focus.
*   Respect `RULES.md`, `WIP_GUIDELINES.md`, and `BRANCHING_MODEL.md`.
*   Update `BITACORA.md` after completing any work.

## Applying This Template

1.  Copy the repository structure.
2.  Replace placeholder content.
3.  Instantiate `projects/fsharpaot.project.yml`.
4.  Log initial state in `PLAN.md`, `STATUS.md`, and `BITACORA.md`.

## Validating Canon Compliance

*   [x] All required files exist.
*   [x] The project YAML matches the schema.
*   [ ] `BITACORA.md` is updated chronologically.
*   [ ] Active branches follow `BRANCHING_MODEL.md`.
*   [ ] Testing commitments match `TESTING.md`.
