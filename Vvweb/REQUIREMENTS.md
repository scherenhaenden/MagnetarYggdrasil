# Requirements for V (vweb)

## Functional Requirements

### Must-Have
1.  **Users API:** Create, Read (List/Single), Update, Delete users.
    *   Fields: `id`, `username`, `email`.
2.  **Tasks API:** Create, Read (List/Single), Update, Patch (Done), Delete tasks.
    *   Fields: `id`, `user_id`, `title`, `description`, `done`.
3.  **System API:** `GET /health` returning status and version.
4.  **Database:** SQLite3 with auto-schema creation and Foreign Keys enabled.

## Non-Functional Requirements

### Must-Have
1.  **Architecture:** Layered (Controller, Service, Repository, Model).
2.  **Performance:** Comparable to other implementations.
3.  **Testing:** 100% Code Coverage target.
4.  **Language:** V (latest stable).
5.  **Framework:** vweb.
