module model;

import vibe.data.json;
import std.typecons;

struct User {
    @optional long id;
    string name;
    string email;
}

struct Task {
    @optional long id;
    long user_id;
    string title;
    string description;
    bool done;
}

struct CreateUserRequest {
    string name;
    string email;
}

struct UpdateUserRequest {
    @optional string name;
    @optional string email;
}

struct CreateTaskRequest {
    string title;
    string description;
}

struct UpdateTaskRequest {
    @optional string title;
    @optional string description;
    @optional Nullable!bool done;
}
