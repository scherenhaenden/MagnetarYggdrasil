# Build Stage
FROM mcr.microsoft.com/dotnet/sdk:10.0-preview AS build
WORKDIR /app

# Install native dependencies for AOT (clang, zlib1g-dev are standard for .NET AOT on Linux)
RUN apt-get update && apt-get install -y clang zlib1g-dev

# Copy project files
COPY . .

# Restore and Publish
# Note: FSharpAot.fsproj is in the root of the copy
RUN dotnet publish FSharpAot.fsproj -c Release -o /app/publish

# Runtime Stage
FROM mcr.microsoft.com/dotnet/runtime-deps:10.0-preview
WORKDIR /app
COPY --from=build /app/publish .

# Ensure the database file is not copied from build (it should be created at runtime)
# but if we need a pre-seeded one, we'd handle it here.
# The app creates it if missing.

ENV ASPNETCORE_URLS=http://+:8080
EXPOSE 8080
ENTRYPOINT ["./FSharpAot"]
