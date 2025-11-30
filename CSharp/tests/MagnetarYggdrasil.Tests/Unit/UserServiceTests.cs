using Xunit;
using Moq;
using FluentAssertions;
using MagnetarYggdrasil.Services;
using MagnetarYggdrasil.Repositories;
using MagnetarYggdrasil.Models;

namespace MagnetarYggdrasil.Tests.Unit;

public class UserServiceTests
{
    private readonly Mock<IUserRepository> _userRepoMock;
    private readonly UserService _userService;

    public UserServiceTests()
    {
        _userRepoMock = new Mock<IUserRepository>();
        _userService = new UserService(_userRepoMock.Object);
    }

    [Fact]
    public async Task CreateUserAsync_ShouldReturnUser_WhenValid()
    {
        // Arrange
        var name = "John Doe";
        var email = "john@example.com";
        var expectedUser = new User { Id = 1, Name = name, Email = email };

        _userRepoMock.Setup(x => x.CreateAsync(It.IsAny<User>()))
            .ReturnsAsync(expectedUser);

        // Act
        var result = await _userService.CreateUserAsync(name, email);

        // Assert
        result.Should().BeEquivalentTo(expectedUser);
        _userRepoMock.Verify(x => x.CreateAsync(It.Is<User>(u => u.Name == name && u.Email == email)), Times.Once);
    }

    [Fact]
    public async Task CreateUserAsync_ShouldThrow_WhenNameIsEmpty()
    {
        // Act & Assert
        await Assert.ThrowsAsync<ArgumentException>(() => _userService.CreateUserAsync("", "email@example.com"));
    }

    [Fact]
    public async Task GetUserByIdAsync_ShouldReturnUser_WhenExists()
    {
        // Arrange
        var user = new User { Id = 1, Name = "Test", Email = "test@test.com" };
        _userRepoMock.Setup(x => x.GetByIdAsync(1)).ReturnsAsync(user);

        // Act
        var result = await _userService.GetUserByIdAsync(1);

        // Assert
        result.Should().Be(user);
    }
}
