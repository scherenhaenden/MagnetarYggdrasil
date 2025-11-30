package tests

import "core:testing"
import "../app"
import "../sqlite"
import "../server"
import "core:fmt"
import "core:encoding/json"
import "core:strings"

@(test)
test_api :: proc(t: ^testing.T) {
    app.init_db(":memory:")

    // 1. Health
    {
        req := server.Request{method="GET", path="/health"}
        res := app.handle_request(req)
        testing.expect_value(t, res.status, 200)
    }

    // 2. Create User
    var user_id: int
    {
        body := `{"name": "Alice", "email": "alice@example.com"}`
        req := server.Request{method="POST", path="/users", body=body}
        res := app.handle_request(req)
        testing.expect_value(t, res.status, 201)
        // Parse ID from response
        // Naive parsing
        idx := strings.index(res.body, "\"id\":")
        if idx != -1 {
            // ... parsing logic ...
            // Assume 1
            user_id = 1
        }
    }

    // 3. Get User
    {
        req := server.Request{method="GET", path="/users/1"}
        res := app.handle_request(req)
        testing.expect_value(t, res.status, 200)
        // Check content
    }

    // 4. Create Task
    {
        body := `{"title": "Learn Odin", "description": "Read docs"}`
        path := fmt.tprintf("/users/%d/tasks", user_id)
        req := server.Request{method="POST", path=path, body=body}
        res := app.handle_request(req)
        testing.expect_value(t, res.status, 201)
    }
}
