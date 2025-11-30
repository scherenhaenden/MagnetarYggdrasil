using System.Text.Json.Serialization;
using MagnetarYggdrasil.Data;
using MagnetarYggdrasil.Endpoints;
using MagnetarYggdrasil.Repositories;
using MagnetarYggdrasil.Services;

var builder = WebApplication.CreateSlimBuilder(args);

// Add services
builder.Services.AddSingleton<IDbConnectionFactory>(_ =>
    new SqliteConnectionFactory("Data Source=app.db;Foreign Keys=True"));
builder.Services.AddSingleton<DatabaseInitializer>();
builder.Services.AddScoped<IUserRepository, UserRepository>();
builder.Services.AddScoped<ITaskRepository, TaskRepository>();
builder.Services.AddScoped<UserService>();
builder.Services.AddScoped<TaskService>();

// JSON configuration for AOT
builder.Services.ConfigureHttpJsonOptions(options =>
{
    options.SerializerOptions.TypeInfoResolverChain.Insert(0, AppJsonSerializerContext.Default);
});

var app = builder.Build();

// Initialize DB
var initializer = app.Services.GetRequiredService<DatabaseInitializer>();
initializer.Initialize();

// Map endpoints
app.MapUserEndpoints();
app.MapTaskEndpoints();
app.MapGet("/health", () => Results.Ok(new { status = "healthy" }));

app.Run();

[JsonSerializable(typeof(MagnetarYggdrasil.Models.User))]
[JsonSerializable(typeof(IEnumerable<MagnetarYggdrasil.Models.User>))]
[JsonSerializable(typeof(MagnetarYggdrasil.Models.TaskItem))]
[JsonSerializable(typeof(IEnumerable<MagnetarYggdrasil.Models.TaskItem>))]
[JsonSerializable(typeof(MagnetarYggdrasil.Endpoints.CreateUserRequest))]
[JsonSerializable(typeof(MagnetarYggdrasil.Endpoints.UpdateUserRequest))]
[JsonSerializable(typeof(MagnetarYggdrasil.Endpoints.CreateTaskRequest))]
[JsonSerializable(typeof(MagnetarYggdrasil.Endpoints.UpdateTaskRequest))]
[JsonSerializable(typeof(object))] // For anonymous types like health check
internal partial class AppJsonSerializerContext : JsonSerializerContext
{
}

public partial class Program { }
