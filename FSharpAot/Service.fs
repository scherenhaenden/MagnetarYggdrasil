namespace FSharpAot.Service

open FSharpAot.Core
open FSharpAot.Repository

type Service(repo: IRepository) =

    member this.CreateUser(req: CreateUserRequest) =
        repo.CreateUser(req)

    member this.GetUsers() =
        repo.GetUsers()

    member this.GetUserById(id: int) =
        repo.GetUserById(id)

    member this.UpdateUser(id: int, req: UpdateUserRequest) =
        repo.UpdateUser(id, req)

    member this.DeleteUser(id: int) =
        repo.DeleteUser(id)

    member this.CreateTask(userId: int, req: CreateTaskRequest) =
        repo.CreateTask(userId, req)

    member this.GetTasksByUserId(userId: int) =
        match repo.GetUserById(userId) with
        | Some _ -> Some (repo.GetTasksByUserId(userId))
        | None -> None

    member this.GetTaskById(id: int) =
        repo.GetTaskById(id)

    member this.UpdateTask(id: int, req: UpdateTaskRequest) =
        repo.UpdateTask(id, req)

    member this.MarkTaskDone(id: int) =
        repo.MarkTaskDone(id)

    member this.DeleteTask(id: int) =
        repo.DeleteTask(id)
