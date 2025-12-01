using System.Text.Json.Serialization;
using FSharpAot.Core;

namespace FSharpAot.Json
{
    [JsonSerializable(typeof(User))]
    [JsonSerializable(typeof(User[]))]
    [JsonSerializable(typeof(CreateUserRequest))]
    [JsonSerializable(typeof(UpdateUserRequest))]
    [JsonSerializable(typeof(FSharpAot.Core.Task))]
    [JsonSerializable(typeof(FSharpAot.Core.Task[]))]
    [JsonSerializable(typeof(CreateTaskRequest))]
    [JsonSerializable(typeof(UpdateTaskRequest))]
    [JsonSerializable(typeof(HealthResponse))]
    public partial class AppJsonSerializerContext : JsonSerializerContext
    {
    }
}
