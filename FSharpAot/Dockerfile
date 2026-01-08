# Build Stage
FROM mcr.microsoft.com/dotnet/sdk:10.0 AS build
WORKDIR /app

# Install native dependencies for AOT (clang, zlib1g-dev are standard for .NET AOT on Linux)
RUN apt-get update && apt-get install -y clang zlib1g-dev

# Copy project files for caching dependencies
COPY *.fsproj *.csproj *.slnx ./
COPY Core/*.fsproj Core/
COPY Json/*.csproj Json/
COPY Tests/*.fsproj Tests/

# Restore dependencies
RUN dotnet restore

# Copy the rest of the source code
COPY . .

# Publish
RUN dotnet publish FSharpAot.fsproj -c Release -o /app/publish --no-restore

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
