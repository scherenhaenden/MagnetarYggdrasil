# 🌌 **MagnetarYggdrasil**

### *One project — many universes. A single codebase implemented in every major backend technology.*

---

MagnetarYggdrasil exists to **compare Web APIs across programming languages** — not theoretically, but in reality — by implementing **the same identical application**, same database, same endpoints, same architecture, same test suite, across all ecosystems.

> ⚡ The goal is simple:
> Build *one program* that exists simultaneously in *multiple technology worlds*
> so we can finally see *which ones shine, which ones bleed and why.*

Yggdrasil = the tree that connects worlds.
Magnetar = the densest star in the universe.
**MagnetarYggdrasil = a system that lives in all technological worlds simultaneously.**

---

# 📊 Project Readiness Matrix

This matrix tracks the current status of each project implementation within MagnetarYggdrasil.
The goal is for every project to reach "Business Equal" status, meaning it fully implements the API, Database, and Architecture standards, and passes all tests.

| Ecosystem | Implemented (Code Exists) | Implements Canonical Model | Tests Written | Docker Written | Test Status | Business Equal Ready | Notes |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :--- |
| **Ada/SPARK** | ✅ | ✅ | ✅ | ❌ | ❓ | ❌ | Pending |
| **Bun** | ❌ | ❌ | ❌ | ❌ | ❓ | ❌ | Pending |
| **C (kore.io)** | ✅ | ❌ | ✅ | ✅ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/CKore.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/CKore.yml) | ❌ | Validating Business Equal |
| **C# (.NET 10 Native AOT)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/CSharp.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/CSharp.yml) | ❌ | Implementation exists, needs Canonical files, needs Docker |
| **C++ (Drogon)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Cpp.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Cpp.yml) | ❌ | Implementation exists, needs Canonical files, needs Docker |
| **Carbon** | ❌ | ❌ | ❌ | ❌ | ❓ | ❌ | Planned |
| **Clojure (Ring)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/ClojureRing.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/ClojureRing.yml) | ❌ | Implementation exists, needs Docker |
| **Crystal (Kemal)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/CrystalKemal.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/CrystalKemal.yml) | ❌ | Implementation exists, needs Docker |
| **D (vibe.d)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/DVibe.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/DVibe.yml) | ❌ | Implementation exists, needs Canonical files, needs Tests, needs Docker |
| **Elixir (Phoenix)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Elixir.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Elixir.yml) | ❌ | Implementation exists, needs Canonical files, needs Docker |
| **Erlang (Cowboy)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Erlang.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Erlang.yml) | ❌ | Implementation exists, needs Canonical files, needs Docker |
| **F# (.NET AOT)** | ✅ | ❌ | ❌ | ❌ | ❓ | ❌ | Pending |
| **Fortran** | ❌ | ❌ | ❌ | ❌ | ❓ | ❌ | Pending |
| **Go (Gin)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Go.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Go.yml) | ❌ | Implementation exists, needs Canonical files, needs Docker |
| **Haskell (Servant)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Haskell.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Haskell.yml) | ❌ | Implementation exists, needs Canonical files, needs Docker |
| **Java (GraalVM)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/JavaGraalVM.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/JavaGraalVM.yml) | ❌ | Implementation exists, needs Canonical files, needs Docker |
| **Java (Spring Boot)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/JavaSpring.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/JavaSpring.yml) | ❌ | Implementation exists, needs Canonical files, needs Docker |
| **Julia (Genie)** | ✅ | ✅ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/JuliaGenie.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/JuliaGenie.yml) | ❌ | Implementation exists, needs Docker |
| **Kotlin (Ktor)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Kotlin.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Kotlin.yml) | ❌ | Implementation exists, needs Canonical files, needs Docker |
| **Lua (OpenResty)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/LuaOpenResty.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/LuaOpenResty.yml) | ❌ | Implementation exists, needs Docker |
| **Nim (Jester)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/NimJester.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/NimJester.yml) | ❌ | Implementation exists, needs Canonical files, needs Docker |
| **Node.js (Fastify)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/NodeNestJs.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/NodeNestJs.yml) | ❌ | Implementation exists, needs Canonical files, needs Docker |
| **OCaml (Dream)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/OCamlDream.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/OCamlDream.yml) | ❌ | Implementation exists, needs Docker |
| **Odin (manual)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Odin.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Odin.yml) | ❌ | Implementation exists, needs Canonical files, needs Docker |
| **PHP (Symfony)** | ✅ | ✅ | ✅ | ❌ | ❓ | ❌ | Implementation exists, needs Docker |
| **Python (FastAPI)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/PythonFastApi.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/PythonFastApi.yml) | ❌ | Implementation exists, needs Canonical files, needs Docker |
| **Racket** | ❌ | ❌ | ❌ | ❌ | ❓ | ❌ | Pending |
| **Ruby (Rails)** | ✅ | ✅ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/RubyInRails.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/RubyInRails.yml) | ❌ | Implementation exists, needs Docker |
| **Rust (Actix/Axum)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Rust.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Rust.yml) | ❌ | Implementation exists, needs Docker |
| **Scala (Akka)** | ✅ | ✅ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/ScalaAkka.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/ScalaAkka.yml) | ❌ | Implementation exists, needs Docker |
| **Swift (Vapor)** | ✅ | ❌ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Swift.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Swift.yml) | ❌ | Implementation exists, needs Canonical files, needs Docker |
| **Unison** | ❌ | ❌ | ❌ | ❌ | ❓ | ❌ | Planned |
| **V (vweb)** | ✅ | ✅ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Vvweb.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Vvweb.yml) | ❌ | Implementation exists, needs Docker |
| **WebAssembly** | ❌ | ❌ | ❌ | ❌ | ❓ | ❌ | Pending |
| **Zig std.http** | ✅ | ✅ | ✅ | ❌ | [![Status](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Zig.yml/badge.svg)](https://github.com/scherenhaenden/MagnetarYggdrasil/actions/workflows/Zig.yml) | ❌ | Implementation exists, needs Docker |

**Legend:**
*   ✅ **Implemented**: Source code directory exists and contains code.
*   ✅ **Implements Canonical Model**: Contains `PLAN.md`, `BITACORA.md`, `RULES.md`, etc., and follows the governance structure.
*   ✅ **Tests Written**: Unit/Integration tests exist in the codebase.
*   ✅ **Docker Written**: `Dockerfile` exists for the project.
*   ✅ **Business Equal Ready**: Implements all API endpoints, Database schema, Architecture, passes 100% tests, AND has Dockerfile.

**Action Plan:**
To move a project to "Business Equal Ready", it must first adopt the "Canonical Model" (governance files), write Tests, write Dockerfile, and then verify its code against the Requirements.

---

## 🏛 The Magnetar Canonical Project Model

Every project within MagnetarYggdrasil MUST follow the **Magnetar Canonical Project Model**. This ensures consistency not just in code, but in governance, planning, and documentation.

If you are starting a new implementation or updating an existing one, you must refer to the Canonical Model prompts:

👉 **[Canonical Model Prompt List (CANONICAL_MODEL_PROMPTS.md)](./CANONICAL_MODEL_PROMPTS.md)**

Use these prompts to generate the required documentation (`PLAN.md`, `BITACORA.md`, `RULES.md`, etc.) for your specific language implementation.

---

## ⚖️ "Business Equal" Criteria

For a project to be considered **Business Equal** and valid for testing/benchmarking, it **MUST** rigorously adhere to the following checklist. Deviations are not allowed, as they invalid the scientific comparison.

### 1. 🔌 API Interface Compliance (Strict)

The application MUST expose the following HTTP JSON endpoints with the exact status codes and payload formats.

#### **Users Resource**
*   `POST /users`
    *   **Input:** JSON `{ "username": "string", "email": "string" }`
    *   **Output:** JSON `{ "id": integer, "username": "string", "email": "string" }` (HTTP 201)
    *   **Error:** HTTP 400 if invalid.
*   `GET /users`
    *   **Output:** JSON array of users `[ { ... }, { ... } ]` (HTTP 200)
*   `GET /users/{id}`
    *   **Output:** JSON user object (HTTP 200)
    *   **Error:** HTTP 404 if not found.
*   `PUT /users/{id}`
    *   **Input:** JSON `{ "username": "string" }` (Partial updates allowed)
    *   **Output:** JSON user object (HTTP 200)
*   `DELETE /users/{id}`
    *   **Output:** HTTP 204 No Content.

#### **Tasks Resource**
*   `POST /users/{id}/tasks`
    *   **Input:** JSON `{ "title": "string", "description": "string" }`
    *   **Output:** JSON task object `{ "id": integer, "user_id": integer, "title": "...", "description": "...", "done": boolean }` (HTTP 201)
*   `GET /users/{id}/tasks`
    *   **Output:** JSON array of tasks for that user (HTTP 200)
*   `GET /tasks/{id}`
    *   **Output:** JSON task object (HTTP 200)
*   `PUT /tasks/{id}`
    *   **Input:** JSON `{ "title": "...", "description": "..." }`
    *   **Output:** Updated task object (HTTP 200)
*   `PATCH /tasks/{id}/done`
    *   **Input:** None (or JSON `{ "done": true }`)
    *   **Output:** Updated task object with `done=true` (HTTP 200)
*   `DELETE /tasks/{id}`
    *   **Output:** HTTP 204 No Content.

#### **System**
*   `GET /health`
    *   **Output:** JSON `{ "status": "ok", "version": "1.0.0" }` (HTTP 200)

---

### 2. 🗄 Database Compliance (SQLite)

*   **Engine:** SQLite3 MUST be used.
*   **Auto-Creation:** The application MUST check for the existence of the database file on startup. If missing, it MUST create it and apply the schema automatically.
*   **Foreign Keys:** Foreign Key constraints MUST be enabled (`PRAGMA foreign_keys = ON;`).
*   **Schema:**
    *   `users` table: `id` (PK, Auto Inc), `username` (Text, Unique), `email` (Text, Unique).
    *   `tasks` table: `id` (PK, Auto Inc), `user_id` (FK -> users.id, On Delete Cascade), `title` (Text), `description` (Text), `done` (Boolean/Int).

---

### 3. 🏗 Architecture Compliance

The code MUST follow a specific Layered Architecture to ensure code metrics (LOC, complexity) are comparable.

*   **Controller Layer (Handlers):** Responsible ONLY for HTTP request parsing, calling the Service, and HTTP response formatting. No business logic here.
*   **Service Layer:** Contains the business logic (validations, calls to repository).
*   **Repository Layer:** Responsible ONLY for SQL queries and database interaction. No HTTP knowledge here.
*   **Models/DTOs:** Plain objects used to transfer data between layers.

---

### 4. 🧪 Testing Compliance

*   **Coverage:** 100% Code Coverage is the target.
*   **Unit Tests:** Mock the database/repository to test Service logic.
*   **Integration Tests:** Use an in-memory SQLite database or a temporary file to test the full Repository -> Service -> Controller flow.
*   **End-to-End Tests:** A script that boots the server and runs `curl` or HTTP client requests against the live API.

---

### 5. 📜 Governance Compliance

The project directory MUST contain:
*   `README_RUN.md`: Instructions to build and run the specific project.
*   `PLAN.md`, `BITACORA.md`, `STATUS.md`, etc., generated from the **Canonical Model Prompts**.

---

## 🏆 Final Comparative Table (The One Everyone Looks For)

| Language / Framework         | Throughput (Req/s)   | High Load Latency   | RAM         | DX - Development Speed         | Is it viable for Web API?        |
| ---------------------------- | -------------------- | ------------------- | ----------- | ------------------------------ | -------------------------------- |
| **Rust (Actix/Axum)**        | 🚀🚀🚀🚀🚀 *Extreme* | <1ms                | 🔥 Very low | ⭐⭐ Difficult                   | **Yes. Best total performance.** |
| **C++ (Drogon)**             | 🚀🚀🚀🚀🚀 *Extreme* | <1ms                | Low         | ⭐ Very difficult               | Yes, niche critical systems      |
| **C (kore.io)**              | 🚀🚀🚀🚀🚀 (theoretical)| ultra low           | minimal     | 💀 DX Nightmare                | Not generally recommended        |
| **Go (Gin/Fiber)**           | 🚀🚀🚀🚀 (very high) | 1–2ms               | low         | ⭐⭐⭐⭐ very good                 | **Cloud standard**               |
| **C# .NET 10 AOT**           | 🚀🚀🚀🚀 (very high) | ~1ms                | 30–50MB     | ⭐⭐⭐⭐⭐ excellent                | **Rivals Go**                    |
| **Java (Spring)**            | 🚀🚀🚀 (high)        | medium              | high        | ⭐⭐⭐ medium                     | Solid Enterprise                 |
| **Java (GraalVM)**           | 🚀🚀🚀🚀 (high+)     | low                 | low         | ⭐⭐⭐ medium                     | Excellent microservices          |
| **Swift (Vapor)**            | 🚀🚀🚀 (high)        | low/medium          | medium      | ⭐⭐⭐ niche                      | viable but small                 |
| **Node.js (Fastify/NestJS)** | 🚀🚀 (medium-high)   | medium              | medium      | ⭐⭐⭐⭐⭐ *BEST DX*                | Ideal for rapid dev              |
| **Python (FastAPI)**         | 🚀 (low-medium)      | high                | medium      | ⭐⭐⭐⭐⭐ *fastest coding*         | production with care             |
| **Zig std.http**             | 🚀🚀🚀🚀 (very high) | <2ms                | very low    | ⭐⭐⭐ medium                     | excellent systems                |
| **D (vibe.d)**               | 🚀🚀🚀🚀 (high)      | low                 | medium/low  | ⭐⭐⭐⭐ very good                 | underrated                       |
| **Nim (Jester/Prologue)**    | 🚀🚀🚀 (high)        | low                 | low         | ⭐⭐⭐⭐ pleasant                  | surprising                       |
| **Odin (manual)**            | 🚀🚀🚀 (variable)    | depends on dev      | low         | ⭐ difficult                    | requires expert hands            |
| **Carbon**                   | 🟡 Experimental      | -                   | -           | -                              | 🟡 Planned                       |

(See `MATRIX.md` for the full list of planned and active projects)

---

## 📂 Repository Structure

```
/magnetar-yggdrasil
   /Rust
   /Cpp
   /C
   /Zig
   /Go
   /CSharp
   /JavaSpring
   /Swift
   /DVibe
   ... (see MATRIX.md)
```

Each folder contains its own `README_RUN.md` and Canonical Model documentation.

---

# ✨ Purpose of this project

This is **not** tutorial code.
This is research.

We are answering **the real question:**

> What language is truly best for backend systems
> when ALL conditions are equal?

You will feel performance.
You will feel friction.
You will discover what Google, Meta, Amazon and Microsoft already learned.

And *you will know it for real — because you built all worlds yourself.*
