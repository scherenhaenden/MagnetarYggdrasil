import Fluent
import FluentSQLiteDriver
import Vapor

// configures your application
public func configure(_ app: Application) async throws {
    // uncomment to serve files from /Public folder
    // app.middleware.use(FileMiddleware(publicDirectory: app.directory.publicDirectory))

    app.databases.use(.sqlite(.file("db.sqlite")), as: .sqlite)

    app.migrations.add(CreateUser())
    app.migrations.add(CreateTask())

    // Auto-migrate on startup for this project as per requirements
    try await app.autoMigrate()

    // register routes
    try routes(app)
}
