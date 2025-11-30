using MagnetarYggdrasil.Models;

namespace MagnetarYggdrasil.Repositories;

public interface ITaskRepository
{
    Task<TaskItem> CreateAsync(TaskItem task);
    Task<TaskItem?> GetByIdAsync(int id);
    Task<IEnumerable<TaskItem>> GetByUserIdAsync(int userId);
    Task<bool> UpdateAsync(TaskItem task);
    Task<bool> MarkAsDoneAsync(int id);
    Task<bool> DeleteAsync(int id);
}
