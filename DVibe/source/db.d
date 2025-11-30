module db;

import d2sqlite3;
import std.stdio;

// In a real high-perf vibe.d app we might want a connection pool or handle blocking better,
// but for this implementation we use a global database connection as d2sqlite3 is thread-safe (serialized mode).
// However, since vibe.d uses fibers, blocking on DB will block the thread.
// We will rely on the fact that for this exercise we keep it simple.

__gshared Database db;

void initDB(string dbPath) {
    db = Database(dbPath);

    // Enable foreign keys
    db.run("PRAGMA foreign_keys = ON;");

    // Create tables
    db.run(`
        CREATE TABLE IF NOT EXISTS users (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            name TEXT NOT NULL,
            email TEXT NOT NULL UNIQUE
        );
    `);

    db.run(`
        CREATE TABLE IF NOT EXISTS tasks (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            user_id INTEGER NOT NULL,
            title TEXT NOT NULL,
            description TEXT NOT NULL,
            done BOOLEAN NOT NULL DEFAULT 0,
            FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE
        );
    `);

    // Indexes
    db.run("CREATE INDEX IF NOT EXISTS idx_users_email ON users(email);");
    db.run("CREATE INDEX IF NOT EXISTS idx_tasks_user_id ON tasks(user_id);");
}

Database getDB() {
    return db;
}
