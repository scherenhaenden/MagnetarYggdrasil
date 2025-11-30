# Canonical Project Model of Lua (OpenResty)

## Purpose
This project is an implementation of the MagnetarYggdrasil specification using Lua and OpenResty. It serves to compare the performance, developer experience, and resource usage of OpenResty against other backend technologies when implementing the exact same requirements. It follows the Magnetar standard for documentation, planning, and governance to ensure a fair and scientific comparison.

## How to Use This Repository

1.  **Clone the canonical model:** This repository contains the source code and documentation.
2.  **Configuration:** Check `projects/lua-openresty.project.yml` for the project definition.
3.  **Documentation:** Refer to the files listed below for detailed information.
4.  **Running:** See `README_RUN.md` for instructions on how to build and run the project.

## Project Contents

| File | Purpose |
| :--- | :--- |
| `PLAN.md` | Project tasks & milestones. |
| `BITACORA.md` | Chronological logbook of events and decisions. |
| `REQUIREMENTS.md` | Functional & non-functional specifications. |
| `ARCHITECTURE.md` | System/module structure. |
| `RULES.md` | Naming & workflow standards. |
| `STATUS.md` | Health summary & progress stats. |
| `TESTING.md` | Test coverage & reporting rules. |
| `BLOCKERS.md` | Documented blockers. |
| `BRANCHING_MODEL.md` | Git branching strategy. |
| `WIP_GUIDELINES.md` | Work-In-Progress limits. |

## Progress Model Overview
The project tracks progress through milestones and tasks. State transitions (`planned` → `in_progress` → `in_review` → `done`) are strictly followed. Every state change is recorded in `BITACORA.md`.

## YAML Project Schema
The `projects/lua-openresty.project.yml` file contains the canonical machine-readable schema with metadata, stakeholders, milestones, tasks, and risks.

## Guidance for AI Collaborators
*   Parse `projects/lua-openresty.project.yml`.
*   Use `PLAN.md` and `STATUS.md` to determine focus.
*   Respect `RULES.md`, `WIP_GUIDELINES.md`, and `BRANCHING_MODEL.md`.
*   Update `BITACORA.md` after completing any work.

## Validating Canon Compliance
*   All required files exist.
*   The project YAML matches the schema.
*   `BITACORA.md` is updated chronologically.
