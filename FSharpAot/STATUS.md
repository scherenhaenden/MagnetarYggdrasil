# Status of F# (.NET AOT)

## Progress Summary
**Progress:** 5%

## Current Milestones
*   `ms-01` Project Initialization: **In Progress**
*   `ms-02` Core Implementation: **Planned**
*   `ms-03` API & AOT: **Planned**

## Risks and Mitigations
*   **Risk:** Native AOT compatibility issues with some libraries.
    *   **Mitigation:** Use raw ADO.NET and AOT-safe JSON serialization (`System.Text.Json` source generation).
*   **Risk:** F# tooling support for AOT might be less mature than C#.
    *   **Mitigation:** Stick to simple F# constructs and standard libraries.
