using System.Data;
using Microsoft.Data.Sqlite;
using MagnetarYggdrasil.Models;
using MagnetarYggdrasil.Data;

namespace MagnetarYggdrasil.Repositories;

public class UserRepository(IDbConnectionFactory connectionFactory) : IUserRepository
{
    private readonly IDbConnectionFactory _connectionFactory = connectionFactory;

    public async Task<User> CreateAsync(User user)
    {
        using var connection = (SqliteConnection)_connectionFactory.CreateConnection();
        await connection.OpenAsync();

        using var command = connection.CreateCommand();
        command.CommandText = @"
            INSERT INTO Users (Name, Email)
            VALUES (@Name, @Email);
            SELECT last_insert_rowid();";

        command.Parameters.AddWithValue("@Name", user.Name);
        command.Parameters.AddWithValue("@Email", user.Email);

        var id = (long)(await command.ExecuteScalarAsync())!;
        user.Id = (int)id;
        return user;
    }

    public async Task<User?> GetByIdAsync(int id)
    {
        using var connection = (SqliteConnection)_connectionFactory.CreateConnection();
        await connection.OpenAsync();

        using var command = connection.CreateCommand();
        command.CommandText = "SELECT * FROM Users WHERE Id = @Id";
        command.Parameters.AddWithValue("@Id", id);

        using var reader = await command.ExecuteReaderAsync();
        if (await reader.ReadAsync())
        {
            return MapUser(reader);
        }
        return null;
    }

    public async Task<IEnumerable<User>> GetAllAsync()
    {
        using var connection = (SqliteConnection)_connectionFactory.CreateConnection();
        await connection.OpenAsync();

        using var command = connection.CreateCommand();
        command.CommandText = "SELECT * FROM Users";

        var users = new List<User>();
        using var reader = await command.ExecuteReaderAsync();
        while (await reader.ReadAsync())
        {
            users.Add(MapUser(reader));
        }
        return users;
    }

    public async Task<bool> UpdateAsync(User user)
    {
        using var connection = (SqliteConnection)_connectionFactory.CreateConnection();
        await connection.OpenAsync();

        using var command = connection.CreateCommand();
        command.CommandText = "UPDATE Users SET Name = @Name, Email = @Email WHERE Id = @Id";
        command.Parameters.AddWithValue("@Name", user.Name);
        command.Parameters.AddWithValue("@Email", user.Email);
        command.Parameters.AddWithValue("@Id", user.Id);

        var rowsAffected = await command.ExecuteNonQueryAsync();
        return rowsAffected > 0;
    }

    public async Task<bool> DeleteAsync(int id)
    {
        using var connection = (SqliteConnection)_connectionFactory.CreateConnection();
        await connection.OpenAsync();

        using var command = connection.CreateCommand();
        command.CommandText = "DELETE FROM Users WHERE Id = @Id";
        command.Parameters.AddWithValue("@Id", id);

        var rowsAffected = await command.ExecuteNonQueryAsync();
        return rowsAffected > 0;
    }

    private static User MapUser(System.Data.Common.DbDataReader reader)
    {
        return new User
        {
            Id = reader.GetInt32(0),
            Name = reader.GetString(1),
            Email = reader.GetString(2)
        };
    }
}
