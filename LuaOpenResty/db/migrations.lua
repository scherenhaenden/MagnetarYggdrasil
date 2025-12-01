local DB = require("db.db")

local Migrations = {}

function Migrations.migrate()
    local db = DB.connect()

    local users_table = [[
        CREATE TABLE IF NOT EXISTS users (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            username TEXT NOT NULL UNIQUE,
            email TEXT NOT NULL UNIQUE
        );
    ]]

    local tasks_table = [[
        CREATE TABLE IF NOT EXISTS tasks (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            user_id INTEGER NOT NULL,
            title TEXT NOT NULL,
            description TEXT NOT NULL,
            done INTEGER NOT NULL DEFAULT 0,
            FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE
        );
    ]]

    local res, err = db:exec(users_table)
    if not res then error("Failed to create users table: " .. err) end

    res, err = db:exec(tasks_table)
    if not res then error("Failed to create tasks table: " .. err) end

    db:close()
    return true
end

return Migrations
