# Bun (Canonical Project Model)

## Purpose
This project is an implementation of the Magnetar Canonical Project Model using the **Bun** runtime. It serves to demonstrate the capabilities of Bun in a standardized environment, facilitating comparison with other technologies.

## How to Use This Repository
1.  **Clone the canonical model.**
2.  **Copy and fill out the `projects/_template.project.yml` file** (see `projects/Bun.project.yml`).
3.  **Replicate the required documentation set.** (This directory contains all required files).
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
| `BRANCHING_MODEL.md` | Governance references for branching. |
| `WIP_GUIDELINES.md` | Governance references for WIP limits. |

## Progress Model Overview
The project tracks progress through milestones and tasks. State transitions (`planned` → `in_progress` → `in_review` → `done`) are strictly monitored, and every state change is recorded in `BITACORA.md`.

## YAML Project Schema
The `projects/Bun.project.yml` file contains the canonical machine-readable schema with metadata, stakeholders, milestones, tasks, and risks.

## Guidance for AI Collaborators
*   Parse `projects/Bun.project.yml`.
*   Use `PLAN.md` and `STATUS.md` to determine focus.
*   Respect `RULES.md`, `WIP_GUIDELINES.md`, and `BRANCHING_MODEL.md`.
*   Update `BITACORA.md` after completing any work.

## Applying This Template
1.  Copy the repository structure.
2.  Replace placeholder content with project-specific details.
3.  Instantiate and validate a project YAML file.
4.  Establish initial milestones and log the initial state in `PLAN.md`, `STATUS.md`, and `BITACORA.md`.

## Validating Canon Compliance
*   [x] All required files exist.
*   [x] The project YAML matches the schema.
*   [x] `BITACORA.md` is updated chronologically.
*   [x] Active branches follow the `BRANCHING_MODEL.md` rules.
*   [x] Testing commitments and blocker processes match `TESTING.md` and `BLOCKERS.md`.
