# How to Run D (vibe.d) Implementation

## Prerequisites

- D compiler (DMD, LDC, or GDC)
- DUB (D package manager)
- SQLite3 development libraries

## Running

```bash
cd DVibe
dub run
```

## Testing

```bash
cd DVibe
dub test
```

## Architecture

- **Web Framework**: vibe.d
- **Database**: SQLite3 (via d2sqlite3)
- **Structure**:
    - `source/app.d`: Main entry and routing.
    - `source/controller.d`: Request handlers.
    - `source/service.d`: Business logic.
    - `source/repository.d`: Database access.
    - `source/model.d`: Data structures.
    - `source/db.d`: Database initialization and connection.

## Notes

- The implementation uses a global database connection.
- `d2sqlite3` is used in blocking mode (standard wrapper).
