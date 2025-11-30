# Prompt List for Replicating the Magnetar Canonical Project Model

Use the following prompts to generate the file structure and base content for a new project that follows the Magnetar Canonical Project Model. Each prompt is designed to create a specific file, defining its purpose, structure, and required content.

---

### 1. `README.md`

**Prompt:**

"Create a `README.md` file that serves as the definitive introduction to our project, following the Magnetar Canonical Project Model standard. The `README.md` must meet the following requirements:

1.  **Main Title:** 'Canonical Project Model of [Project Name]'.
2.  **Purpose:** A clear and concise section explaining why the project exists, what problem it solves, and its value. It should state that the project follows the Magnetar standard for documentation, planning, and governance.
3.  **How to Use This Repository:** A numbered guide instructing users and collaborators on how to get started, including the following steps:
    *   Clone the canonical model.
    *   Copy and fill out the `projects/_template.project.yml` file.
    *   Replicate the required documentation set.
    *   Follow the WIP, branching, and blocker rules.
    *   Consult the example project to resolve questions.
4.  **Project Contents:** A table listing the key documentation files and their purpose. The table should include:
    *   `PLAN.md`: Project tasks & milestones.
    *   `BITACORA.md`: Chronological logbook.
    *   `REQUIREMENTS.md`: Functional & non-functional specs.
    *   `ARCHITECTURE.md`: System/module structure.
    *   `RULES.md`: Naming & workflow standards.
    *   `STATUS.md`: Health summary & progress stats.
    *   `TESTING.md`: Test coverage & reporting rules.
    *   `BLOCKERS.md`: Documented blockers & escalation paths.
    *   Also mention `BRANCHING_MODEL.md` and `WIP_GUIDELINES.md` as governance references.
5.  **Progress Model Overview:** An explanation of how the project tracks progress through milestones, tasks, and state transitions (`planned` → `in_progress` → `in_review` → `done`). It should emphasize that every state change is recorded in `BITACORA.md`.
6.  **YAML Project Schema:** A description of the `projects/_template.project.yml` file, explaining that it contains the canonical machine-readable schema with metadata, stakeholders, milestones, tasks, and risks.
7.  **Guidance for AI Collaborators:** A set of guidelines for AI agents, stating that they must:
    *   Parse the project YAML file.
    *   Use `PLAN.md` and `STATUS.md` to determine focus.
    *   Respect `RULES.md`, `WIP_GUIDELINES.md`, and `BRANCHING_MODEL.md`.
    *   Update `BITACORA.md` after completing any work.
8.  **Architecture Diagram (Optional but Recommended):** A simple text-based flowchart showing how governance standards inform planning, which in turn feeds execution artifacts and examples.
9.  **Applying This Template:** Clear steps for applying the template to a new project, including:
    *   Copy the repository structure.
    *   Replace placeholder content with project-specific details.
    *   Instantiate and validate a project YAML file.
    *   Establish initial milestones and log the initial state in `PLAN.md`, `STATUS.md`, and `BITACORA.md`.
10. **Validating Canon Compliance:** A checklist to confirm that a project follows the Magnetar canon, ensuring that:
    *   All required files exist.
    *   The project YAML matches the schema.
    *   `BITACORA.md` is updated chronologically.
    *   Active branches follow the `BRANCHING_MODEL.md` rules.
    *   Testing commitments and blocker processes match `TESTING.md` and `BLOCKERS.md`."

---

### 2. `RULES.md`

**Prompt:**

"Create a `RULES.md` file that establishes the fundamental rules and workflow standards for our project, based on the Magnetar Canonical Model. The file should be clear, directive, and cover the following areas:

1.  **Main Title:** 'Canonical Ruleset of [Project Name]'.
2.  **Introduction:** A statement indicating that these rules codify the Magnetar standard and that the entire project must comply, unless a formal exception is documented in `BITACORA.md`.
3.  **Naming Conventions:** A list of naming rules for:
    *   **Repositories:** `magnetar-<domain>-<descriptor>`.
    *   **Branches:** `<type>/<short-description>`, where `type` is `feature`, `fix`, `chore`, `experiment`, or `hotfix`.
    *   **Tasks and Blockers:** `kebab-case` (e.g., `task-202`, `blocker-db-outage`).
    *   **YAML Keys:** `lower_snake_case`.
    *   **File Names:** Must mirror those in the canonical repository.
4.  **Required Files:** An explicit list of files that **must** be included in every Magnetar project:
    *   `README.md`, `PLAN.md`, `BITACORA.md`, `REQUIREMENTS.md`, `ARCHITECTURE.md`, `RULES.md`, `STATUS.md`, `TESTING.md`, `BLOCKERS.md`.
    *   `BRANCHING_MODEL.md`, `WIP_GUIDELINES.md`, `CONTRIBUTING.md`.
    *   `projects/<project>.project.yml`.
    *   Mention that any omission requires an explicit exemption logged in `BITACORA.md`.
5.  **Branching Conventions:** Clear rules for branch management:
    *   `master`: Immutable release line; merges require successful CI and documentation updates.
    *   `develop` (optional): Aggregates completed features before stabilization.
    *   `feature` branches: Originate from `master` or `develop` and must be rebased before merging.
    *   `hotfix` branches: Start from `master` and must trigger a `STATUS.md` update upon completion.
    *   Each Pull Request must reference the tasks it affects and include `BITACORA.md` entries.
6.  **Allowed Task States:** A list of the only valid task states:
    1.  `planned`
    2.  `ready`
    3.  `in_progress`
    4.  `in_review`
    5.  `blocked`
    6.  `done`
    *   Include a description of allowed state transitions (e.g., `ready` → `in_progress` when work begins).
7.  **Work-In-Progress (WIP) Constraints:**
    *   WIP Limit: Specify the maximum number of `in_progress` tasks an individual or AI agent can have simultaneously (e.g., two).
    *   Exceptions: State that exceeding the limit requires approval documented in `WIP_GUIDELINES.md` and `BITACORA.md`.
8.  **Blocker Lifecycle:** A numbered process for managing blockers:
    1.  **Discovery:** Log in `BLOCKERS.md` with ID, description, severity, owner, and timestamp.
    2.  **Assessment:** Update risks in `STATUS.md` and note mitigation ideas in `BITACORA.md`.
    3.  **Escalation:** Define an escalation policy if not resolved within a specified timeframe (e.g., one business day).
    4.  **Resolution:** Document the solution steps in `BITACORA.md` and update the blocker status to `resolved`.
    5.  **Retrospective:** Capture lessons learned.
9.  **Documentation Discipline:**
    *   `BITACORA.md`: Must chronologically record every state change, decision, or exception.
    *   `STATUS.md`: Must be updated at least once per day or after each PR merge.
    *   `PLAN.md`: Is the source of truth for milestones and task assignments.
10. **AI Agent Responsibilities:**
    *   Parse the project YAML file before acting.
    *   Do not open PRs without confirming the task state is `in_review`.
    *   Document assumptions in `BITACORA.md` when uncertain.
11. **Compliance and Enforcement:**
    *   Mention that Continuous Integration (CI) should validate the presence and structure of required files.
    *   State that periodic audits will be conducted."

---

### 3. `PLAN.md`

**Prompt:**

"Create a `PLAN.md` file that serves as the central planning document for our project, following the structure of the Magnetar Canonical Model. The file should be clear, tabular, and easy for both humans and machines to parse.

1.  **Main Title:** 'Canonical Plan of [Project Name]'.
2.  **Introduction:** a brief sentence stating that this plan captures the project's milestones, tasks, estimates, and status, and that its structure must be kept intact.
3.  **Milestones Overview Table:** A table with the following columns:
    *   `Milestone ID`: A unique identifier (e.g., `ms-01`).
    *   `Name`: A descriptive name for the milestone (e.g., 'Project Initiation').
    *   `Target Date`: The target completion date.
    *   `Description`: A brief description of what the milestone entails.
    *   `Completion Criteria`: Clear, measurable criteria defining when the milestone is complete.
    *   Fill the table with 1-3 example milestones relevant to the project's start.
4.  **Task Backlog Table:** A detailed table listing all project tasks, with the following columns:
    *   `Task ID`: A unique identifier (e.g., `task-101`).
    *   `Milestone`: The `Milestone ID` the task belongs to.
    *   `Title`: A concise, descriptive title for the task.
    *   `Owner`: The person or team responsible for the task.
    *   `Effort (pts)`: An effort estimate in story points.
    *   `Weight (%)`: The percentage weight of the task relative to the total effort (optional but recommended).
    *   `State`: The current state of the task, which must be one of the allowed states in `RULES.md` (`planned`, `ready`, `in_progress`, `blocked`, `in_review`, `done`).
    *   `Notes`: Any relevant additional notes (e.g., links to PRs or documents).
    *   Fill the table with 3-5 example tasks with different states to illustrate the format.
5.  **Effort Summary:** A section summarizing the project's total effort status:
    *   **Total effort:** Sum of all effort points.
    *   **Completed:** Sum of points for tasks in the `done` state.
    *   **In progress:** Sum of points for tasks in the `in_progress` state.
    *   **Remaining:** Sum of points for tasks in other states.
6.  **State Definitions:** A list that clearly defines each of the allowed task states (`planned`, `ready`, `in_progress`, `blocked`, `in_review`, `done`) to eliminate any ambiguity.
7.  **Change Management:** A final note stating that this document must be updated whenever tasks change state or scope, and that the changes must be reflected in the project's YAML file and in `BITACORA.md`."

---

### 4. `BITACORA.md`

**Prompt:**

"Create a `BITACORA.md` file that functions as an immutable, chronological log of all significant project events. This file is crucial for auditing, transparency, and tracking the project's history.

1.  **Main Title:** 'Logbook of [Project Name]'.
2.  **Introduction:** A brief description stating that this document is a logbook or journal that records decisions, state changes, discoveries, and key events in reverse chronological order (most recent first).
3.  **Entry Format:** Specify that each entry in the logbook must follow a strict and consistent format:
    *   **Timestamp:** A timestamp in `YYYY-MM-DD HH:MM Z` format (e.g., `2024-01-15 10:30 UTC`).
    *   **Author:** The name of the person or AI agent making the entry.
    *   **Entry:** A clear and concise description of the event.
4.  **Entry Categories:** Indicate that entries should be self-descriptive and may include, but are not limited to, the following event categories:
    *   **State Change:** A task's state change (e.g., `task-101: state changed from 'in_progress' to 'in_review'`).
    *   **Decision:** A key decision made (e.g., `Decision: Adopted PostgreSQL over MySQL for the main database`).
    *   **Blocker:** Creation or resolution of a blocker (e.g., `Blocker-001 created: API access denied`).
    *   **Discovery:** A significant finding or new idea (e.g., `Discovery: Found a critical vulnerability in the authentication library`).
    *   **PR Merge:** Merging of a Pull Request (e.g., `PR #12 merged: Implemented user authentication feature`).
    *   **Exception:** A documented deviation from the canonical rules.
5.  **Example Entries:** Provide 3-4 example entries that demonstrate the format and variety of events to be recorded. For example:

    ---
    **Timestamp:** 2024-01-15 14:00 UTC
    **Author:** Jules
    **Entry:** `task-102`: state changed from `in_progress` to `in_review`. Awaiting approval from governance board.

    ---
    **Timestamp:** 2024-01-15 11:45 UTC
    **Author:** Kai
    **Entry:** `Blocker-001` created: Staging environment deployment is failing due to authentication key mismatch. `task-201` is now `blocked`.

    ---
    **Timestamp:** 2024-01-14 09:00 UTC
    **Author:** Mira
    **Entry:** Decision: The team has decided to use React for the frontend framework based on the prototype's performance. `REQUIREMENTS.md` updated.
    ---

6.  **Immutability:** Add a final note highlighting that the logbook should not be altered. Corrections should be made by adding a new entry that clarifies or corrects a previous one, rather than modifying the history."

---

### 5. `REQUIREMENTS.md`

**Prompt:**

"Create a `REQUIREMENTS.md` file that clearly and exhaustively defines the project's functional and non-functional requirements.

1.  **Main Title:** 'Requirements for [Project Name]'.
2.  **Sections for Functional and Non-Functional Requirements:** Divide the document into clear sections for each type of requirement.
3.  **Prioritization and Labeling:** Use labels like 'Must-Have', 'Should-Have', 'Could-Have', and 'Won't-Have' (MoSCoW) to prioritize requirements."

---

### 6. `ARCHITECTURE.md`

**Prompt:**

"Create an `ARCHITECTURE.md` file that describes the high-level structure of the system, its components, and key design decisions.

1.  **Main Title:** 'Architecture of [Project Name]'.
2.  **Architecture Diagram:** Include a high-level diagram (can be in text format or a link to an image) showing the main system components and their interactions.
3.  **Component Descriptions:** Provide a brief description of each main component, its responsibility, and the technologies used."

---

### 7. `STATUS.md`

**Prompt:**

"Create a `STATUS.md` file that offers a quick, up-to-date overview of the project's status.

1.  **Main Title:** 'Status of [Project Name]'.
2.  **Progress Summary:** Display the overall project progress, ideally with a progress bar or completion percentage.
3.  **Current Milestones:** List the current milestones and their status (e.g., 'In Progress', 'Completed').
4.  **Risks and Mitigations:** Enumerate the identified risks and mitigation strategies."

---

### 8. `TESTING.md`

**Prompt:**

"Create a `TESTING.md` file that describes the project's testing strategy, the types of tests to be performed, and the acceptance criteria.

1.  **Main Title:** 'Testing Strategy for [Project Name]'.
2.  **Types of Tests:** Describe the different types of tests that will be conducted (e.g., unit, integration, end-to-end).
3.  **Code Coverage:** Specify the target code coverage for automated tests.
4.  **Bug Reporting Process:** Define the process for reporting and tracking bugs."

---

### 9. `BLOCKERS.md`

**Prompt:**

"Create a `BLOCKERS.md` file to document and manage all impediments hindering the project's progress.

1.  **Main Title:** 'Blockers for [Project Name]'.
2.  **Blockers Table:** Use a table to track blockers, with columns for 'ID', 'Description', 'Creation Date', 'Owner', and 'Status' ('Active', 'Resolved').
3.  **Escalation Process:** Describe the process to follow if a blocker is not resolved within a certain timeframe."

---

### 10. `BRANCHING_MODEL.md`, `CONTRIBUTING.md`, `WIP_GUIDELINES.md`

**Prompt:**

"Create the following governance files:

*   **`BRANCHING_MODEL.md`:** Describe the project's Git branching model (e.g., GitFlow).
*   **`CONTRIBUTING.md`:** Provide guidelines for contributors, including how to set up the development environment and the process for submitting Pull Requests.
*   **`WIP_GUIDELINES.md`:** Define the Work-In-Progress (WIP) policies and limits for the team."

---

### 11. `projects/_template.project.yml`

**Prompt:**

"Create a YAML template file named `_template.project.yml` in the `projects` directory. This file will serve as the canonical machine-readable schema for project configuration.

The file should include sections for:

*   **`metadata`:** Project name, description, start date.
*   **`stakeholders`:** A list of key stakeholders and their roles.
*   **`milestones`:** A list of milestones with their IDs, names, and target dates.
*   **`tasks`:** A list of tasks with their IDs, titles, owners, and initial states.
*   **`risks`:** A registry of potential risks.
*   **`reporting`:** Hooks or configurations for automated reports."
