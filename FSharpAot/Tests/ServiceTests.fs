module FSharpAot.Tests.ServiceTests

open Xunit
open Moq
open FSharpAot.Core
open FSharpAot.Repository
open FSharpAot.Service

[<Fact>]
let ``CreateUser should call repo and return user`` () =
    let mockRepo = new Mock<IRepository>()
    let req = { Username = "jules"; Email = "jules@example.com" }
    let expectedUser = { Id = 1; Username = "jules"; Email = "jules@example.com" }

    mockRepo.Setup(fun r -> r.CreateUser(req)).Returns(expectedUser) |> ignore

    let service = new Service(mockRepo.Object)
    let result = service.CreateUser(req)

    Assert.Equal(expectedUser, result)
    mockRepo.Verify((fun r -> r.CreateUser(req)), Times.Once)

[<Fact>]
let ``GetUserById should return user when exists`` () =
    let mockRepo = new Mock<IRepository>()
    let user = { Id = 1; Username = "jules"; Email = "jules@example.com" }

    mockRepo.Setup(fun r -> r.GetUserById(1)).Returns(Some user) |> ignore

    let service = new Service(mockRepo.Object)
    let result = service.GetUserById(1)

    Assert.True(result.IsSome)
    Assert.Equal(user, result.Value)

[<Fact>]
let ``GetUserById should return None when not exists`` () =
    let mockRepo = new Mock<IRepository>()
    mockRepo.Setup(fun r -> r.GetUserById(1)).Returns(None) |> ignore

    let service = new Service(mockRepo.Object)
    let result = service.GetUserById(1)

    Assert.True(result.IsNone)
