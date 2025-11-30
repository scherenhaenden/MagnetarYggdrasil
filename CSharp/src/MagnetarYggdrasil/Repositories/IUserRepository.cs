using MagnetarYggdrasil.Models;

namespace MagnetarYggdrasil.Repositories;

public interface IUserRepository
{
    Task<User> CreateAsync(User user);
    Task<User?> GetByIdAsync(int id);
    Task<IEnumerable<User>> GetAllAsync();
    Task<bool> UpdateAsync(User user);
    Task<bool> DeleteAsync(int id);
}
