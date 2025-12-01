namespace FSharpAot

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open FSharpAot.Repository
open FSharpAot.Service
open FSharpAot.Handlers
open FSharpAot.Core
open FSharpAot.Json
open System.Text.Json

// Marker class for tests
type Program = class end

module ProgramEntry =
    [<EntryPoint>]
    let main args =
        let builder = WebApplication.CreateSlimBuilder(args)

        // Configure JSON AOT
        builder.Services.ConfigureHttpJsonOptions(fun options ->
            options.SerializerOptions.TypeInfoResolverChain.Insert(0, AppJsonSerializerContext.Default)
        ) |> ignore

        // DI
        builder.Services.AddSingleton<IRepository>(fun _ ->
            // Check env var or config, default to "Data Source=app.db"
            let dbName = "app.db"
            let connStr = $"Data Source={dbName}"
            new SqliteRepository(connStr) :> IRepository
        ) |> ignore
        builder.Services.AddSingleton<Service>() |> ignore

        let app = builder.Build()

        // Initialize DB
        let repo = app.Services.GetRequiredService<IRepository>()
        repo.Initialize()

        // Routes
        let service = app.Services.GetRequiredService<Service>()
        Handlers.mapRoutes app service

        app.Run()
        0
