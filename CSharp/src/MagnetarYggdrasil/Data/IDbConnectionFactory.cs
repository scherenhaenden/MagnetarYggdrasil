using System.Data;
using Microsoft.Data.Sqlite;

namespace MagnetarYggdrasil.Data;

public interface IDbConnectionFactory
{
    IDbConnection CreateConnection();
}

public class SqliteConnectionFactory(string connectionString) : IDbConnectionFactory
{
    private readonly string _connectionString = connectionString;

    public IDbConnection CreateConnection()
    {
        return new SqliteConnection(_connectionString);
    }
}
