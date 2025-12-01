import { Database } from "bun:sqlite";

export class DB {
    private db: Database;

    constructor(filename: string = ":memory:") {
        this.db = new Database(filename);
        this.init();
    }

    private init() {
        this.db.run(`
            CREATE TABLE IF NOT EXISTS users (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                name TEXT NOT NULL,
                email TEXT NOT NULL UNIQUE
            );
        `);

        this.db.run(`
            CREATE TABLE IF NOT EXISTS tasks (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                user_id INTEGER NOT NULL,
                title TEXT NOT NULL,
                description TEXT NOT NULL,
                is_done BOOLEAN NOT NULL DEFAULT 0,
                FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE
            );
        `);
    }

    getDatabase(): Database {
        return this.db;
    }

    close() {
        this.db.close();
    }
}
