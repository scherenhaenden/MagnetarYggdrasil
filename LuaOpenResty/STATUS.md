# Status of Lua (OpenResty)

## Progress Summary
**Progress:** 5%

## Current Milestones
*   `ms-01`: Project Setup (In Progress)
*   `ms-02`: Implementation (Planned)
*   `ms-03`: Testing (Planned)
*   `ms-04`: Finalization (Planned)

## Risks and Mitigations
*   **Risk:** Finding suitable SQLite3 driver for OpenResty that supports blocking/non-blocking correctly or just works well.
    *   **Mitigation:** Use `ljsqlite3` or `lua-sqlite3` compatible with LuaJIT.
