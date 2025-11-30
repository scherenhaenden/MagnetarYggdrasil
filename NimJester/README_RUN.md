# Nim (Jester) Implementation

## Prerequisites
- Nim installed (tested with 1.6+ or 2.0+)
- `nimble` package manager (usually comes with Nim)

## Setup
1. Install dependencies:
   ```bash
   nimble install -d
   ```

## Running
Run the application:
```bash
nimble run
```
Or manually:
```bash
nim c -r src/main.nim
```

## Testing
Run tests:
```bash
nimble test
```

## Architecture
- **Framework**: Jester
- **Database**: SQLite (via `std/db_sqlite`)
- **JSON**: `std/json`

The application auto-creates the `magnetar.db` SQLite file on startup.
