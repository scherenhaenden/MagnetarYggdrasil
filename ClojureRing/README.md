# Clojure Ring Implementation

This project implements the MagnetarYggdrasil specification using Clojure with the Ring standard and Reitit router.

## 🚀 How to Run

See [README_RUN.md](./README_RUN.md) for instructions.

## 🏗 Architecture

Follows the standard layered architecture:
- **Handler**: `clojure_ring.handler` (HTTP layer)
- **Service**: `clojure_ring.service` (Business Logic)
- **Repository**: `clojure_ring.repository` (Data Access)
- **DB**: `clojure_ring.db` (Database Connection)

## 🧪 Testing

100% Coverage is required.
Run tests with `clj -M:test -m clojure.test.runner` (assuming a runner alias/script) or just via standard clojure test tools.
