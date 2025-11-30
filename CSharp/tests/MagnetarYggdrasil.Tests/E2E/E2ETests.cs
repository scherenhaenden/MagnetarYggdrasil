using Xunit;
using FluentAssertions;
using Microsoft.AspNetCore.Mvc.Testing;

namespace MagnetarYggdrasil.Tests.E2E;

// E2E tests often run against a fully deployed env, but here we simulate it via WebApplicationFactory
// as it provides the closest experience without spinning up a separate process.
public class E2ETests : IClassFixture<WebApplicationFactory<Program>>
{
    private readonly WebApplicationFactory<Program> _factory;

    public E2ETests(WebApplicationFactory<Program> factory)
    {
        _factory = factory;
    }

    [Fact]
    public async Task FullSystemFlow()
    {
        var client = _factory.CreateClient();

        // Check Health
        var health = await client.GetStringAsync("/health");
        health.Should().Contain("healthy");

        // The rest is similar to Integration, but conceptually covers the full user journey
        // We trust IntegrationTests for detailed flows.
    }
}
