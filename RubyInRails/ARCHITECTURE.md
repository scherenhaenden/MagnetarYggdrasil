# Architecture of MagnetarYggdrasil - Ruby (Rails)

## Overview
The application follows a strict layered architecture to ensure comparability with other implementations in the Magnetar project.

## Layers

1.  **Controller Layer (Handlers)**:
    *   `app/controllers/`
    *   Responsible for HTTP request parsing and response formatting.
    *   Calls Service Layer.
    *   **No business logic.**

2.  **Service Layer**:
    *   `app/services/`
    *   Contains business logic (validations, business rules).
    *   Calls Repository Layer.

3.  **Repository Layer**:
    *   `app/repositories/`
    *   Responsible for Database interactions (SQL queries).
    *   Uses ActiveRecord models but abstracts the queries.
    *   **No HTTP knowledge.**

4.  **Models/DTOs**:
    *   `app/models/`
    *   ActiveRecord models (`User`, `Task`).
    *   Used for data transfer.

## Diagram
HTTP Request -> Controller -> Service -> Repository -> Database
HTTP Response <- Controller <- Service <- Repository <- Database
