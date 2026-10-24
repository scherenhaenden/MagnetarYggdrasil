namespace FSharpAot.Core

open System.Text.Json.Serialization

// Domain Models
[<CLIMutable>]
type User = {
    [<JsonPropertyName("id")>]
    Id: int
    [<JsonPropertyName("username")>]
    Username: string
    [<JsonPropertyName("email")>]
    Email: string
}

[<CLIMutable>]
type Task = {
    [<JsonPropertyName("id")>]
    Id: int
    [<JsonPropertyName("user_id")>]
    UserId: int
    [<JsonPropertyName("title")>]
    Title: string
    [<JsonPropertyName("description")>]
    Description: string
    [<JsonPropertyName("done")>]
    Done: bool
}

// DTOs
[<CLIMutable>]
type CreateUserRequest = {
    [<JsonPropertyName("username")>]
    Username: string
    [<JsonPropertyName("email")>]
    Email: string
}

[<CLIMutable>]
type UpdateUserRequest = {
    [<JsonPropertyName("username")>]
    Username: string
}

[<CLIMutable>]
type CreateTaskRequest = {
    [<JsonPropertyName("title")>]
    Title: string
    [<JsonPropertyName("description")>]
    Description: string
}

[<CLIMutable>]
type UpdateTaskRequest = {
    [<JsonPropertyName("title")>]
    Title: string
    [<JsonPropertyName("description")>]
    Description: string
}

[<CLIMutable>]
type HealthResponse = {
    [<JsonPropertyName("status")>]
    Status: string
    [<JsonPropertyName("version")>]
    Version: string
}
