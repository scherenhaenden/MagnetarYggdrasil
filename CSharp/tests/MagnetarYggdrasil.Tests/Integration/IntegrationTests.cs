using Xunit;
using FluentAssertions;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using MagnetarYggdrasil.Data;
using MagnetarYggdrasil.Models;
using System.Net.Http.Json;
using System.Net;

namespace MagnetarYggdrasil.Tests.Integration;

public class IntegrationTests : IClassFixture<WebApplicationFactory<Program>>
{
    private readonly WebApplicationFactory<Program> _factory;

    public IntegrationTests(WebApplicationFactory<Program> factory)
    {
        _factory = factory.WithWebHostBuilder(builder =>
        {
            builder.ConfigureServices(services =>
            {
                // In a real scenario, we might swap DB to a test DB or in-memory SQLite
                // For now, we assume it uses the default flow which creates app.db
                // Ideally, use a unique DB name per test or transaction rollback
                services.AddSingleton<IDbConnectionFactory>(_ =>
                    new SqliteConnectionFactory("Data Source=test_app.db;Foreign Keys=True"));
            });
        });

        // Ensure DB is initialized
        using var scope = _factory.Services.CreateScope();
        var init = scope.ServiceProvider.GetRequiredService<DatabaseInitializer>();
        init.Initialize();

        // Clean up data
        using var conn = scope.ServiceProvider.GetRequiredService<IDbConnectionFactory>().CreateConnection();
        conn.Open();
        using var cmd = conn.CreateCommand();
        cmd.CommandText = "DELETE FROM Tasks; DELETE FROM Users;";
        cmd.ExecuteNonQuery();
    }

    [Fact]
    public async Task CreateUser_And_GetUsers_Flow()
    {
        var client = _factory.CreateClient();

        // Create User
        var user = new { Name = "Integration User", Email = "int@test.com" };
        var createResponse = await client.PostAsJsonAsync("/users", user);
        createResponse.EnsureSuccessStatusCode();
        var createdUser = await createResponse.Content.ReadFromJsonAsync<User>();

        createdUser.Should().NotBeNull();
        createdUser!.Name.Should().Be(user.Name);

        // Get Users
        var getResponse = await client.GetAsync("/users");
        getResponse.EnsureSuccessStatusCode();
        var users = await getResponse.Content.ReadFromJsonAsync<List<User>>();

        users.Should().Contain(u => u.Id == createdUser.Id);
    }

    [Fact]
    public async Task Task_Lifecycle_Flow()
    {
        var client = _factory.CreateClient();

        // 1. Create User
        var user = new { Name = "Task User", Email = "task@test.com" };
        var userRes = await client.PostAsJsonAsync("/users", user);
        var createdUser = await userRes.Content.ReadFromJsonAsync<User>();

        // 2. Create Task
        var task = new { Title = "My Task", Description = "Desc" };
        var taskRes = await client.PostAsJsonAsync($"/users/{createdUser!.Id}/tasks", task);
        taskRes.EnsureSuccessStatusCode();
        var createdTask = await taskRes.Content.ReadFromJsonAsync<TaskItem>();

        createdTask.Should().NotBeNull();
        createdTask!.UserId.Should().Be(createdUser.Id);

        // 3. Get Task
        var getTaskRes = await client.GetAsync($"/tasks/{createdTask.Id}");
        getTaskRes.StatusCode.Should().Be(HttpStatusCode.OK);

        // 4. Update Task
        var update = new { Title = "Updated Title" };
        var putRes = await client.PutAsJsonAsync($"/tasks/{createdTask.Id}", update);
        putRes.StatusCode.Should().Be(HttpStatusCode.OK);

        // 5. Mark as Done
        var patchRes = await client.PatchAsync($"/tasks/{createdTask.Id}/done", null);
        patchRes.StatusCode.Should().Be(HttpStatusCode.OK);

        // Verify Done
        var verifyRes = await client.GetAsync($"/tasks/{createdTask.Id}");
        var verifiedTask = await verifyRes.Content.ReadFromJsonAsync<TaskItem>();
        verifiedTask!.IsDone.Should().BeTrue();
        verifiedTask.Title.Should().Be("Updated Title");

        // 6. Delete Task
        var delRes = await client.DeleteAsync($"/tasks/{createdTask.Id}");
        delRes.StatusCode.Should().Be(HttpStatusCode.NoContent);

        // 7. Verify Deleted
        (await client.GetAsync($"/tasks/{createdTask.Id}")).StatusCode.Should().Be(HttpStatusCode.NotFound);
    }
}
