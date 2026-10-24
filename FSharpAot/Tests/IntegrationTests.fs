module FSharpAot.Tests.IntegrationTests

open Xunit
open Microsoft.AspNetCore.Mvc.Testing
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
open System.Net.Http
open System.Net.Http.Json
open FSharpAot.Core
open FSharpAot.Repository
open System.IO
open FSharpAot
open System.Linq

// We need a custom WebApplicationFactory to swap the DB
type CustomWebApplicationFactory() =
    inherit WebApplicationFactory<Program>()

    override this.ConfigureWebHost(builder) =
        builder.ConfigureServices(fun services ->
            // Remove existing IRepository
            let descriptor = services.SingleOrDefault(fun d -> d.ServiceType = typeof<IRepository>)
            if descriptor <> null then
                services.Remove(descriptor) |> ignore

            // Add new IRepository with temporary DB
            services.AddSingleton<IRepository>(fun _ ->
                let dbName = $"test_{System.Guid.NewGuid()}.db"
                let connStr = $"Data Source={dbName}"
                new SqliteRepository(connStr) :> IRepository
            ) |> ignore
        ) |> ignore

[<Fact>]
let ``Health check returns OK`` () =
    use factory = new CustomWebApplicationFactory()
    let client = factory.CreateClient()

    let response = client.GetAsync("/health").Result
    response.EnsureSuccessStatusCode() |> ignore

    let content = response.Content.ReadFromJsonAsync<HealthResponse>().Result
    Assert.Equal("ok", content.Status)

[<Fact>]
let ``Create and Get User`` () =
    use factory = new CustomWebApplicationFactory()
    let client = factory.CreateClient()

    let req : CreateUserRequest = { Username = "integration"; Email = "int@test.com" }
    let response = client.PostAsJsonAsync("/users", req).Result
    response.EnsureSuccessStatusCode() |> ignore

    let createdUser = response.Content.ReadFromJsonAsync<User>().Result
    Assert.Equal(req.Username, createdUser.Username)
    Assert.True(createdUser.Id > 0)

    let getResponse = client.GetAsync($"/users/{createdUser.Id}").Result
    getResponse.EnsureSuccessStatusCode() |> ignore
    let fetchedUser = getResponse.Content.ReadFromJsonAsync<User>().Result
    Assert.Equal(createdUser, fetchedUser)
