# How to Run F# (.NET AOT)

## Prerequisites
*   .NET 10 SDK (or compatible).
*   `clang` and `zlib1g-dev` (for AOT compilation on Linux).

## Build
```bash
dotnet build
```

## Run (Development)
```bash
dotnet run
```

## Publish (Native AOT)
```bash
dotnet publish -c Release
```
The executable will be in `bin/Release/net10.0/linux-x64/publish/`.

## Test
```bash
dotnet test
```
