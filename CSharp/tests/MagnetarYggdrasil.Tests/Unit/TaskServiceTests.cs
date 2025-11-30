using Xunit;
using Moq;
using FluentAssertions;
using MagnetarYggdrasil.Services;
using MagnetarYggdrasil.Repositories;
using MagnetarYggdrasil.Models;

namespace MagnetarYggdrasil.Tests.Unit;

public class TaskServiceTests
{
    private readonly Mock<ITaskRepository> _taskRepoMock;
    private readonly Mock<IUserRepository> _userRepoMock;
    private readonly TaskService _taskService;

    public TaskServiceTests()
    {
        _taskRepoMock = new Mock<ITaskRepository>();
        _userRepoMock = new Mock<IUserRepository>();
        _taskService = new TaskService(_taskRepoMock.Object, _userRepoMock.Object);
    }

    [Fact]
    public async Task CreateTaskAsync_ShouldReturnTask_WhenUserExists()
    {
        // Arrange
        var userId = 1;
        _userRepoMock.Setup(x => x.GetByIdAsync(userId)).ReturnsAsync(new User { Id = userId, Name = "U", Email = "E" });

        var expectedTask = new TaskItem { Id = 1, UserId = userId, Title = "T", Description = "D", IsDone = false };
        _taskRepoMock.Setup(x => x.CreateAsync(It.IsAny<TaskItem>())).ReturnsAsync(expectedTask);

        // Act
        var result = await _taskService.CreateTaskAsync(userId, "T", "D");

        // Assert
        result.Should().BeEquivalentTo(expectedTask);
    }

    [Fact]
    public async Task CreateTaskAsync_ShouldReturnNull_WhenUserDoesNotExist()
    {
        // Arrange
        _userRepoMock.Setup(x => x.GetByIdAsync(1)).ReturnsAsync((User?)null);

        // Act
        var result = await _taskService.CreateTaskAsync(1, "T", "D");

        // Assert
        result.Should().BeNull();
    }
}
