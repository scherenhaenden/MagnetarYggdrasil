require "sqlite3"
require "db"

module App
  class Database
    DB_URL = "sqlite3://./magnetar.db"

    # We will hold the pool in a class variable
    @@pool : DB::Database?

    def self.setup
      # Initialize the pool
      @@pool = DB.open(DB_URL)

      # Perform schema setup using a temporary checkout or just exec on pool
      # db.exec on pool checks out a connection, executes, and releases it.
      pool.exec "PRAGMA foreign_keys = ON;"

      pool.exec <<-SQL
        CREATE TABLE IF NOT EXISTS users (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          username TEXT NOT NULL UNIQUE,
          email TEXT NOT NULL UNIQUE
        );
      SQL

      pool.exec <<-SQL
        CREATE TABLE IF NOT EXISTS tasks (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          user_id INTEGER NOT NULL,
          title TEXT NOT NULL,
          description TEXT NOT NULL,
          done INTEGER NOT NULL DEFAULT 0,
          FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
        );
      SQL
    end

    def self.pool
      @@pool.not_nil!
    end

    def self.connection
      # Returns a checkout connection which must be closed (released) by the caller
      conn = pool.checkout
      conn.exec "PRAGMA foreign_keys = ON;" # Ensure FKs are on for this connection
      conn
    end

    def self.close
      @@pool.try(&:close)
    end
  end
end
