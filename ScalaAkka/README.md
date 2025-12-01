# Canonical Project Model of MagnetarYggdrasil - Scala (Akka)

## Purpose
This project is an implementation of the MagnetarYggdrasil specification using Scala and Akka HTTP. It serves as a benchmark and reference implementation following the Magnetar standard for documentation, planning, and governance.

## How to Use This Repository

1.  **Clone the canonical model:**
    ```bash
    git clone <repository-url>
    cd ScalaAkka
    ```
2.  **Copy and fill out the project YAML:**
    -   Copy `projects/_template.project.yml` to `projects/scala_akka.project.yml`.
    -   Fill in the specific details for this implementation.
3.  **Replicate the required documentation set:**
    -   Ensure `PLAN.md`, `BITACORA.md`, etc., are present and up to date.
4.  **Follow the WIP, branching, and blocker rules:**
    -   Adhere to `RULES.md` and `WIP_GUIDELINES.md`.
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
| `WIP_GUIDELINES.md` | Work-in-progress limits and policies. |

## Progress Model Overview

The project tracks progress through milestones and tasks. Task states transition as follows:
`planned` → `ready` → `in_progress` → `in_review` → `done`

Every state change must be recorded in `BITACORA.md`.

## YAML Project Schema

The `projects/_template.project.yml` file contains the canonical machine-readable schema with metadata, stakeholders, milestones, tasks, and risks. This implementation's specific configuration is in `projects/scala_akka.project.yml`.

## Guidance for AI Collaborators

-   Parse `projects/scala_akka.project.yml`.
-   Use `PLAN.md` and `STATUS.md` to determine focus.
-   Respect `RULES.md`, `WIP_GUIDELINES.md`, and `BRANCHING_MODEL.md`.
-   Update `BITACORA.md` after completing any work.

## Applying This Template

1.  Copy the repository structure.
2.  Replace placeholder content with project-specific details.
3.  Instantiate and validate a project YAML file.
4.  Establish initial milestones and log the initial state in `PLAN.md`, `STATUS.md`, and `BITACORA.md`.

## Validating Canon Compliance

-   [ ] All required files exist.
-   [ ] The project YAML matches the schema.
-   [ ] `BITACORA.md` is updated chronologically.
-   [ ] Active branches follow the `BRANCHING_MODEL.md` rules.
-   [ ] Testing commitments and blocker processes match `TESTING.md` and `BLOCKERS.md`.
