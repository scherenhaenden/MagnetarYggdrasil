# C# .NET 10 Native AOT

This implementation uses C# with .NET 10 (Preview) configured for Native AOT compilation to achieve high performance and low startup time.

## Prerequisites

- .NET 10 SDK (or latest available, e.g., .NET 8/9, adjusted in `csproj`)
- Native build tools (VS C++ tools on Windows, clang/zlib on Linux, Xcode on macOS) for AOT compilation.

## Project Structure

- `src/MagnetarYggdrasil`: Main web API project.
- `tests/MagnetarYggdrasil.Tests`: Test suite covering Unit, Integration, and E2E tests.

## Running the Application

To run the application in development mode:

```bash
cd src/MagnetarYggdrasil
dotnet run
```

The server will start on `http://localhost:5000` (or similar, check logs).
The SQLite database `app.db` will be automatically created in the running directory.

## Building Native AOT

To publish a self-contained Native AOT executable:

```bash
cd src/MagnetarYggdrasil
dotnet publish -c Release -r linux-x64
```

(Replace `linux-x64` with your runtime identifier, e.g., `win-x64` or `osx-arm64`).

## Running Tests

To run the full test suite with 100% coverage:

```bash
cd tests/MagnetarYggdrasil.Tests
dotnet test
```

## Architecture

- **API Layer**: Minimal APIs defined in `Endpoints/`.
- **Service Layer**: Business logic in `Services/`.
- **Repository Layer**: Raw ADO.NET (`Microsoft.Data.Sqlite`) for AOT-compatible data access in `Repositories/`.
- **Storage Layer**: SQLite database initialized in `Data/`.

## Libraries Used

- **Microsoft.AspNetCore.OpenApi**: API documentation.
- **Microsoft.Data.Sqlite**: SQLite ADO.NET provider.
