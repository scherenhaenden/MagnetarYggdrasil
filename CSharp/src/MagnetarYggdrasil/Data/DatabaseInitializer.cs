using System.Data;
using Microsoft.Data.Sqlite;

namespace MagnetarYggdrasil.Data;

public class DatabaseInitializer(IDbConnectionFactory connectionFactory)
{
    private readonly IDbConnectionFactory _connectionFactory = connectionFactory;

    public void Initialize()
    {
        using var connection = (SqliteConnection)_connectionFactory.CreateConnection();
        connection.Open();

        using var command = connection.CreateCommand();

        // Enable foreign keys
        command.CommandText = "PRAGMA foreign_keys = ON;";
        command.ExecuteNonQuery();

        // Create Users table
        command.CommandText = @"
            CREATE TABLE IF NOT EXISTS Users (
                Id INTEGER PRIMARY KEY AUTOINCREMENT,
                Name TEXT NOT NULL,
                Email TEXT NOT NULL
            );";
        command.ExecuteNonQuery();

        // Create Tasks table
        command.CommandText = @"
            CREATE TABLE IF NOT EXISTS Tasks (
                Id INTEGER PRIMARY KEY AUTOINCREMENT,
                UserId INTEGER NOT NULL,
                Title TEXT NOT NULL,
                Description TEXT NOT NULL,
                IsDone INTEGER NOT NULL DEFAULT 0,
                FOREIGN KEY (UserId) REFERENCES Users(Id) ON DELETE CASCADE
            );";
        command.ExecuteNonQuery();

        // Create Indices
        command.CommandText = "CREATE INDEX IF NOT EXISTS IDX_Tasks_UserId ON Tasks(UserId);";
        command.ExecuteNonQuery();
    }
}
