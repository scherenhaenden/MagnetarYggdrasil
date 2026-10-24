# Architecture of Bun Project

## Overview
The application follows a standard layered architecture to ensure separation of concerns and testability.

## Architecture Diagram
Request -> Router -> Handler -> Service -> Repository -> SQLite

## Component Descriptions
1.  **Router:** Maps HTTP requests to the appropriate handlers.
2.  **Handler:** Parses the request, validates input, calls the service, and formats the response.
3.  **Service:** Contains business logic and orchestrates data operations.
4.  **Repository:** Handles direct database interactions (SQL queries).
5.  **SQLite:** The underlying database engine using `bun:sqlite`.
