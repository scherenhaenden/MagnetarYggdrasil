using MagnetarYggdrasil.Services;

namespace MagnetarYggdrasil.Endpoints;

public static class TaskEndpoints
{
    public static void MapTaskEndpoints(this IEndpointRouteBuilder app)
    {
        // GET /users/{id}/tasks
        app.MapGet("/users/{id}/tasks", async (int id, TaskService service) =>
        {
            var tasks = await service.GetTasksByUserIdAsync(id);
            return Results.Ok(tasks);
        });

        // POST /users/{id}/tasks
        app.MapPost("/users/{id}/tasks", async (int id, CreateTaskRequest request, TaskService service) =>
        {
            try
            {
                var task = await service.CreateTaskAsync(id, request.Title, request.Description);
                if (task == null) return Results.NotFound("User not found");
                return Results.Created($"/tasks/{task.Id}", task);
            }
            catch (ArgumentException ex)
            {
                return Results.BadRequest(ex.Message);
            }
        });

        // GET /tasks/{tid}
        app.MapGet("/tasks/{tid}", async (int tid, TaskService service) =>
        {
            var task = await service.GetTaskByIdAsync(tid);
            return task != null ? Results.Ok(task) : Results.NotFound();
        });

        // PUT /tasks/{tid}
        app.MapPut("/tasks/{tid}", async (int tid, UpdateTaskRequest request, TaskService service) =>
        {
            var updated = await service.UpdateTaskAsync(tid, request.Title, request.Description);
            return updated ? Results.Ok() : Results.NotFound();
        });

        // PATCH /tasks/{tid}/done
        app.MapPatch("/tasks/{tid}/done", async (int tid, TaskService service) =>
        {
            var updated = await service.MarkTaskAsDoneAsync(tid);
            return updated ? Results.Ok() : Results.NotFound();
        });

        // DELETE /tasks/{tid}
        app.MapDelete("/tasks/{tid}", async (int tid, TaskService service) =>
        {
            var deleted = await service.DeleteTaskAsync(tid);
            return deleted ? Results.NoContent() : Results.NotFound();
        });
    }
}

public record CreateTaskRequest(string Title, string Description);
public record UpdateTaskRequest(string? Title, string? Description);
