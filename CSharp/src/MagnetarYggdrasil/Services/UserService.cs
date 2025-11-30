using MagnetarYggdrasil.Models;
using MagnetarYggdrasil.Repositories;

namespace MagnetarYggdrasil.Services;

public class UserService(IUserRepository userRepository)
{
    private readonly IUserRepository _userRepository = userRepository;

    public async Task<User> CreateUserAsync(string name, string email)
    {
        // Simple validation
        if (string.IsNullOrWhiteSpace(name))
            throw new ArgumentException("Name cannot be empty");
        if (string.IsNullOrWhiteSpace(email))
            throw new ArgumentException("Email cannot be empty");

        var user = new User { Name = name, Email = email };
        return await _userRepository.CreateAsync(user);
    }

    public async Task<User?> GetUserByIdAsync(int id)
    {
        return await _userRepository.GetByIdAsync(id);
    }

    public async Task<IEnumerable<User>> GetAllUsersAsync()
    {
        return await _userRepository.GetAllAsync();
    }

    public async Task<bool> UpdateUserAsync(int id, string? name, string? email)
    {
        var user = await _userRepository.GetByIdAsync(id);
        if (user == null) return false;

        if (!string.IsNullOrWhiteSpace(name)) user.Name = name;
        if (!string.IsNullOrWhiteSpace(email)) user.Email = email;

        return await _userRepository.UpdateAsync(user);
    }

    public async Task<bool> DeleteUserAsync(int id)
    {
        return await _userRepository.DeleteAsync(id);
    }
}
