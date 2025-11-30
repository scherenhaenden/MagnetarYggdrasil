namespace MagnetarYggdrasil.Models;

public class TaskItem
{
    public int Id { get; set; }
    public int UserId { get; set; }
    public required string Title { get; set; }
    public required string Description { get; set; }
    public bool IsDone { get; set; }
}
