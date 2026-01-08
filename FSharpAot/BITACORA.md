# Logbook of F# (.NET AOT)

## Introduction
This document records decisions, state changes, discoveries, and key events in reverse chronological order.

## Entry Format
*   **Timestamp:** `YYYY-MM-DD HH:MM Z`
*   **Author:** Name
*   **Entry:** Description

## Entries

---
**Timestamp:** 2026-01-08 10:00 UTC
**Author:** GitHub Copilot
**Entry:** Optimized Dockerfile for better layer caching by separating dependency restoration. Added --no-restore to publish command.

---
**Timestamp:** 2025-12-02 18:00 UTC
**Author:** Jules
**Entry:** `task-009` (Create Dockerfile) state changed from `in_progress` to `done`. Multi-stage Dockerfile created for AOT build.

---
**Timestamp:** 2025-12-02 17:00 UTC
**Author:** Jules
**Entry:** `task-008` (Write Integration Tests) state changed from `in_progress` to `done`. Integration tests using TestHost and temporary SQLite DB implemented.

---
**Timestamp:** 2025-12-02 16:00 UTC
**Author:** Jules
**Entry:** `task-007` (Write Unit Tests) state changed from `in_progress` to `done`. Unit tests with Moq for Service layer completed.

---
**Timestamp:** 2025-12-02 15:00 UTC
**Author:** Jules
**Entry:** `task-006` (Configure AOT & Entry Point) state changed from `in_progress` to `done`. Program.fs configured for AOT with CreateSlimBuilder.

---
**Timestamp:** 2025-12-02 14:00 UTC
**Author:** Jules
**Entry:** `task-005` (Implement HTTP Handlers) state changed from `in_progress` to `done`. All API endpoints implemented using Minimal APIs.

---
**Timestamp:** 2025-12-01 12:00 UTC
**Author:** Jules
**Entry:** `task-004` (Implement Service Layer) state changed from `in_progress` to `done`. Service layer with business logic completed.

---
**Timestamp:** 2025-12-01 11:00 UTC
**Author:** Jules
**Entry:** `task-003` (Implement Repository (SQLite)) state changed from `in_progress` to `done`. SQLite repository with ADO.NET implemented.

---
**Timestamp:** 2025-12-01 10:00 UTC
**Author:** Jules
**Entry:** `task-002` (Implement Domain Models) state changed from `in_progress` to `done`. Core models and DTOs defined.

---
**Timestamp:** 2025-11-30 12:45 UTC
**Author:** Jules
**Entry:** `task-001` (Create Directory & Docs) state changed from `in_progress` to `done`. Initial project structure and documentation created.

---
**Timestamp:** 2025-11-30 12:40 UTC
**Author:** Jules
**Entry:** Project initialized. Created `FSharpAot` directory and dotnet project.

---
**Timestamp:** 2025-11-30 12:35 UTC
**Author:** Jules
**Entry:** Installed .NET 10 SDK locally to enable F# AOT development.
