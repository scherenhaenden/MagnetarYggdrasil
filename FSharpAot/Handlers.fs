namespace FSharpAot.Handlers

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing
open FSharpAot.Core
open FSharpAot.Service
open System

module Handlers =

    let mapRoutes (app: WebApplication) (service: Service) =

        // System
        app.MapGet("/health", Func<IResult>(fun () ->
            TypedResults.Ok({ Status = "ok"; Version = "1.0.0" }) :> IResult
        )) |> ignore

        // Users
        app.MapPost("/users", Func<CreateUserRequest, IResult>(fun req ->
            try
                let user = service.CreateUser(req)
                TypedResults.Created($"/users/{user.Id}", user) :> IResult
            with _ ->
                TypedResults.BadRequest() :> IResult
        )) |> ignore

        app.MapGet("/users", Func<IResult>(fun () ->
            let users = service.GetUsers()
            TypedResults.Ok(users) :> IResult
        )) |> ignore

        app.MapGet("/users/{id}", Func<int, IResult>(fun id ->
            match service.GetUserById(id) with
            | Some user -> TypedResults.Ok(user) :> IResult
            | None -> TypedResults.NotFound() :> IResult
        )) |> ignore

        app.MapPut("/users/{id}", Func<int, UpdateUserRequest, IResult>(fun id req ->
            match service.UpdateUser(id, req) with
            | Some user -> TypedResults.Ok(user) :> IResult
            | None -> TypedResults.NotFound() :> IResult
        )) |> ignore

        app.MapDelete("/users/{id}", Func<int, IResult>(fun id ->
            if service.DeleteUser(id) then
                TypedResults.NoContent() :> IResult
            else
                TypedResults.NoContent() :> IResult // Idempotent
        )) |> ignore

        // Tasks
        app.MapPost("/users/{id}/tasks", Func<int, CreateTaskRequest, IResult>(fun id req ->
            match service.CreateTask(id, req) with
            | Some task -> TypedResults.Created($"/tasks/{task.Id}", task) :> IResult
            | None -> TypedResults.NotFound() :> IResult // User not found
        )) |> ignore

        app.MapGet("/users/{id}/tasks", Func<int, IResult>(fun id ->
            match service.GetTasksByUserId(id) with
            | Some tasks -> TypedResults.Ok(tasks) :> IResult
            | None -> TypedResults.NotFound() :> IResult
        )) |> ignore

        app.MapGet("/tasks/{id}", Func<int, IResult>(fun id ->
            match service.GetTaskById(id) with
            | Some task -> TypedResults.Ok(task) :> IResult
            | None -> TypedResults.NotFound() :> IResult
        )) |> ignore

        app.MapPut("/tasks/{id}", Func<int, UpdateTaskRequest, IResult>(fun id req ->
            match service.UpdateTask(id, req) with
            | Some task -> TypedResults.Ok(task) :> IResult
            | None -> TypedResults.NotFound() :> IResult
        )) |> ignore

        app.MapPatch("/tasks/{id}/done", Func<int, IResult>(fun id ->
            match service.MarkTaskDone(id) with
            | Some task -> TypedResults.Ok(task) :> IResult
            | None -> TypedResults.NotFound() :> IResult
        )) |> ignore

        app.MapDelete("/tasks/{id}", Func<int, IResult>(fun id ->
            if service.DeleteTask(id) then
                TypedResults.NoContent() :> IResult
            else
                TypedResults.NoContent() :> IResult
        )) |> ignore
