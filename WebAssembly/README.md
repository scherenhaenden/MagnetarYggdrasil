# WebAssembly (Spin) Implementation

This directory contains the WebAssembly implementation of the MagnetarYggdrasil specification, using **Rust** as the source language and **Fermyon Spin** as the serverless Wasm framework.

## Architecture

The project follows the strict Layered Architecture:

*   **Handlers (`src/handlers.rs`):** HTTP request parsing and response formatting.
*   **Service (`src/service.rs`):** Business logic and validation.
*   **Repository (`src/repository.rs`):** Data access using `spin_sdk::sqlite`.
*   **Models (`src/models.rs`):** Data structures.

## Database

Uses `spin_sdk::sqlite` to interact with a SQLite database. Spin manages the database connection.
For local development, Spin uses a local SQLite file (usually `.spin/sqlite_db.db` or similar).

## Prerequisites

*   Rust (stable)
*   `wasm32-wasi` target: `rustup target add wasm32-wasi`
*   Spin CLI: [Install Spin](https://developer.fermyon.com/spin/install)
