using System.Data;
using Microsoft.Data.Sqlite;
using MagnetarYggdrasil.Models;

namespace MagnetarYggdrasil.Repositories;

public class TaskRepository(IDbConnectionFactory connectionFactory) : ITaskRepository
{
    private readonly IDbConnectionFactory _connectionFactory = connectionFactory;

    public async Task<TaskItem> CreateAsync(TaskItem task)
    {
        using var connection = (SqliteConnection)_connectionFactory.CreateConnection();
        await connection.OpenAsync();

        using var command = connection.CreateCommand();
        command.CommandText = @"
            INSERT INTO Tasks (UserId, Title, Description, IsDone)
            VALUES (@UserId, @Title, @Description, @IsDone);
            SELECT last_insert_rowid();";

        command.Parameters.AddWithValue("@UserId", task.UserId);
        command.Parameters.AddWithValue("@Title", task.Title);
        command.Parameters.AddWithValue("@Description", task.Description);
        command.Parameters.AddWithValue("@IsDone", task.IsDone ? 1 : 0);

        var id = (long)(await command.ExecuteScalarAsync())!;
        task.Id = (int)id;
        return task;
    }

    public async Task<TaskItem?> GetByIdAsync(int id)
    {
        using var connection = (SqliteConnection)_connectionFactory.CreateConnection();
        await connection.OpenAsync();

        using var command = connection.CreateCommand();
        command.CommandText = "SELECT * FROM Tasks WHERE Id = @Id";
        command.Parameters.AddWithValue("@Id", id);

        using var reader = await command.ExecuteReaderAsync();
        if (await reader.ReadAsync())
        {
            return MapTask(reader);
        }
        return null;
    }

    public async Task<IEnumerable<TaskItem>> GetByUserIdAsync(int userId)
    {
        using var connection = (SqliteConnection)_connectionFactory.CreateConnection();
        await connection.OpenAsync();

        using var command = connection.CreateCommand();
        command.CommandText = "SELECT * FROM Tasks WHERE UserId = @UserId";
        command.Parameters.AddWithValue("@UserId", userId);

        var tasks = new List<TaskItem>();
        using var reader = await command.ExecuteReaderAsync();
        while (await reader.ReadAsync())
        {
            tasks.Add(MapTask(reader));
        }
        return tasks;
    }

    public async Task<bool> UpdateAsync(TaskItem task)
    {
        using var connection = (SqliteConnection)_connectionFactory.CreateConnection();
        await connection.OpenAsync();

        using var command = connection.CreateCommand();
        command.CommandText = @"
            UPDATE Tasks
            SET Title = @Title, Description = @Description
            WHERE Id = @Id";
        command.Parameters.AddWithValue("@Title", task.Title);
        command.Parameters.AddWithValue("@Description", task.Description);
        command.Parameters.AddWithValue("@Id", task.Id);

        var rowsAffected = await command.ExecuteNonQueryAsync();
        return rowsAffected > 0;
    }

    public async Task<bool> MarkAsDoneAsync(int id)
    {
        using var connection = (SqliteConnection)_connectionFactory.CreateConnection();
        await connection.OpenAsync();

        using var command = connection.CreateCommand();
        command.CommandText = "UPDATE Tasks SET IsDone = 1 WHERE Id = @Id";
        command.Parameters.AddWithValue("@Id", id);

        var rowsAffected = await command.ExecuteNonQueryAsync();
        return rowsAffected > 0;
    }

    public async Task<bool> DeleteAsync(int id)
    {
        using var connection = (SqliteConnection)_connectionFactory.CreateConnection();
        await connection.OpenAsync();

        using var command = connection.CreateCommand();
        command.CommandText = "DELETE FROM Tasks WHERE Id = @Id";
        command.Parameters.AddWithValue("@Id", id);

        var rowsAffected = await command.ExecuteNonQueryAsync();
        return rowsAffected > 0;
    }

    private static TaskItem MapTask(System.Data.Common.DbDataReader reader)
    {
        return new TaskItem
        {
            Id = reader.GetInt32(0),
            UserId = reader.GetInt32(1),
            Title = reader.GetString(2),
            Description = reader.GetString(3),
            IsDone = reader.GetInt32(4) != 0
        };
    }
}
