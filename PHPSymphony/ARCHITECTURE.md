# Architecture of PHPSymphony

## High-Level Architecture

The **PHPSymphony** project follows a strict **Layered Architecture** typical of Symfony applications.

```
[ HTTP Request ]
       ⬇
[ Front Controller (public/index.php) ]
       ⬇
[ Kernel (Symfony Framework) ]
       ⬇
[ Controller Layer (src/Controller) ]
   - Parses HTTP Request
   - Validates Input (basic)
   - Calls Service
       ⬇
[ Service Layer (src/Service) ]
   - Business Logic
   - Validation
   - Calls Repository
       ⬇
[ Repository Layer (src/Repository) ]
   - Doctrine ORM
   - SQL Queries (SQLite)
       ⬇
[ Database (var/data.db) ]
```

## Component Descriptions

### 1. Controllers (`src/Controller/`)
*   **Responsibility:** Handle incoming HTTP requests, deserialize JSON payloads, invoke business logic in Services, and return JSON responses.
*   **Technology:** Symfony `AbstractController`, `JsonResponse`.
*   **Components:** `UserController`, `TaskController`, `SystemController`.

### 2. Services (`src/Service/`)
*   **Responsibility:** Implement core business logic, ensure data integrity, and coordinate data persistence.
*   **Technology:** Plain PHP Classes.
*   **Components:** `UserService`, `TaskService`.

### 3. Repositories (`src/Repository/`)
*   **Responsibility:** Abstract the data access layer. Perform CRUD operations and complex queries.
*   **Technology:** Doctrine ORM `ServiceEntityRepository`.
*   **Components:** `UserRepository`, `TaskRepository`.

### 4. Entities (`src/Entity/`)
*   **Responsibility:** represent the database schema as objects.
*   **Technology:** Doctrine Attributes.
*   **Components:** `User`, `Task`.

### 5. Database
*   **Technology:** SQLite3.
*   **File:** `var/data.db`.
*   **Management:** Doctrine Migrations (or auto-schema creation on boot).
