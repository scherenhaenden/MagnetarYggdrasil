using MagnetarYggdrasil.Models;
using MagnetarYggdrasil.Repositories;

namespace MagnetarYggdrasil.Services;

public class TaskService(ITaskRepository taskRepository, IUserRepository userRepository)
{
    private readonly ITaskRepository _taskRepository = taskRepository;
    private readonly IUserRepository _userRepository = userRepository;

    public async Task<TaskItem?> CreateTaskAsync(int userId, string title, string description)
    {
        // Verify user exists
        var user = await _userRepository.GetByIdAsync(userId);
        if (user == null) return null; // Or throw exception

        if (string.IsNullOrWhiteSpace(title))
            throw new ArgumentException("Title cannot be empty");

        var task = new TaskItem
        {
            UserId = userId,
            Title = title,
            Description = description,
            IsDone = false
        };
        return await _taskRepository.CreateAsync(task);
    }

    public async Task<TaskItem?> GetTaskByIdAsync(int id)
    {
        return await _taskRepository.GetByIdAsync(id);
    }

    public async Task<IEnumerable<TaskItem>> GetTasksByUserIdAsync(int userId)
    {
        // Ideally verify user exists, but simple listing is fine
        return await _taskRepository.GetByUserIdAsync(userId);
    }

    public async Task<bool> UpdateTaskAsync(int id, string? title, string? description)
    {
        var task = await _taskRepository.GetByIdAsync(id);
        if (task == null) return false;

        if (!string.IsNullOrWhiteSpace(title)) task.Title = title;
        if (description != null) task.Description = description; // Allow clearing description? Assuming yes or just update if provided.

        return await _taskRepository.UpdateAsync(task);
    }

    public async Task<bool> MarkTaskAsDoneAsync(int id)
    {
        return await _taskRepository.MarkAsDoneAsync(id);
    }

    public async Task<bool> DeleteTaskAsync(int id)
    {
        return await _taskRepository.DeleteAsync(id);
    }
}
