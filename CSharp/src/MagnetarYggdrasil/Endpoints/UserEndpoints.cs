using MagnetarYggdrasil.Services;

namespace MagnetarYggdrasil.Endpoints;

public static class UserEndpoints
{
    public static void MapUserEndpoints(this IEndpointRouteBuilder app)
    {
        // GET /users
        app.MapGet("/users", async (UserService service) =>
        {
            return Results.Ok(await service.GetAllUsersAsync());
        });

        // GET /users/{id}
        app.MapGet("/users/{id}", async (int id, UserService service) =>
        {
            var user = await service.GetUserByIdAsync(id);
            return user != null ? Results.Ok(user) : Results.NotFound();
        });

        // POST /users
        app.MapPost("/users", async (CreateUserRequest request, UserService service) =>
        {
            try
            {
                var user = await service.CreateUserAsync(request.Name, request.Email);
                return Results.Created($"/users/{user.Id}", user);
            }
            catch (ArgumentException ex)
            {
                return Results.BadRequest(ex.Message);
            }
        });

        // PUT /users/{id}
        app.MapPut("/users/{id}", async (int id, UpdateUserRequest request, UserService service) =>
        {
            var updated = await service.UpdateUserAsync(id, request.Name, request.Email);
            return updated ? Results.Ok() : Results.NotFound();
        });

        // DELETE /users/{id}
        app.MapDelete("/users/{id}", async (int id, UserService service) =>
        {
            var deleted = await service.DeleteUserAsync(id);
            return deleted ? Results.NoContent() : Results.NotFound();
        });
    }
}

public record CreateUserRequest(string Name, string Email);
public record UpdateUserRequest(string? Name, string? Email);
