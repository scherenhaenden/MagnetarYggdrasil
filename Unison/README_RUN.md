# Run Guide for Unison Implementation

> **⚠️ WORK IN PROGRESS**
> The current codebase is a **simulation/skeleton** and does not yet fully implement the HTTP server or database connections required by the Magnetar specification.

## Prerequisites
*   [Unison Codebase Manager (UCM)](https://www.unison-lang.org/docs/quickstart/)

## Setup
1.  Initialize a codebase:
    ```bash
    ucm init
    ```
2.  Load the source files. You can load them individually or point UCM to the source directory if configured, but manual loading ensures order:
    ```bash
    ucm
    ```
    Inside UCM:
    ```unison
    .> load src/Domain.u
    .> load src/Repository.u
    .> load src/Service.u
    .> load src/Handler.u
    .> load src/Server.u
    .> load src/Main.u
    ```

## Running the Application (Simulation)
From the UCM console:
```unison
.> run main
```

## Running Tests
From the UCM console, load the tests:
```unison
.> load tests/Test.u
.> run test.flow
```
