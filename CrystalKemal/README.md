# Canonical Project Model of MagnetarYggdrasil - Crystal (Kemal)

## Purpose
This project is an implementation of the MagnetarYggdrasil specification using the Crystal language and the Kemal web framework. It exists to benchmark the performance, developer experience, and viability of Crystal/Kemal for backend web development when compared directly against other technologies under identical conditions. It follows the Magnetar standard for documentation, planning, and governance.

## How to Use This Repository

1.  **Clone the canonical model:** This repository contains the source code for the Crystal implementation.
2.  **Replicate the required documentation set:** The `CrystalKemal/` directory contains all necessary documentation files.
3.  **Follow the WIP, branching, and blocker rules:** Refer to `WIP_GUIDELINES.md` and `BRANCHING_MODEL.md`.
4.  **Consult the example project:** Use this project as a reference for other Magnetar implementations.

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
| `WIP_GUIDELINES.md` | Work-in-progress limits. |

## Progress Model Overview
The project tracks progress through milestones, tasks, and state transitions (`planned` → `ready` → `in_progress` → `in_review` → `done`). Every state change is recorded in `BITACORA.md`.

## YAML Project Schema
The `projects/crystalkemal.project.yml` file contains the canonical machine-readable schema with metadata, stakeholders, milestones, tasks, and risks.

## Guidance for AI Collaborators
*   Parse `projects/crystalkemal.project.yml`.
*   Use `PLAN.md` and `STATUS.md` to determine focus.
*   Respect `RULES.md`, `WIP_GUIDELINES.md`, and `BRANCHING_MODEL.md`.
*   Update `BITACORA.md` after completing any work.

## Validating Canon Compliance
*   All required files exist.
*   The project YAML matches the schema.
*   `BITACORA.md` is updated chronologically.
*   Active branches follow the `BRANCHING_MODEL.md` rules.
*   Testing commitments and blocker processes match `TESTING.md` and `BLOCKERS.md`.
