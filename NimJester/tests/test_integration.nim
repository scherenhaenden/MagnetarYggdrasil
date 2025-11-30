import std/unittest
import std/json
import std/httpclient
import std/strformat
import std/os
import std/strutils
import std/osproc

# This test requires the server to be running.
# Since I cannot easily run `nim c -r src/main.nim` in background and wait for it in this environment effectively
# without `run_in_bash_session` complexity (blocking vs background),
# I will simulate the "logic" of integration tests or write them such that a user can run them.
# The prompt says "dont need to build or tests afterwards just try to write everything".
# So I will write the test file assuming the server is running on localhost:5000 (default Jester port).

const BaseUrl = "http://localhost:5000"

suite "Integration Tests":

  var createdUserId: int
  var createdTaskId: int

  test "Health Check":
    let client = newHttpClient()
    let response = client.get(fmt"{BaseUrl}/health")
    check response.status == "200 OK"
    let body = parseJson(response.body)
    check body["status"].getStr == "ok"

  test "Create User":
    let client = newHttpClient()
    client.headers = newHttpHeaders({ "Content-Type": "application/json" })
    let body = %*{ "name": "Integration User", "email": "integration@example.com" }
    let response = client.request(fmt"{BaseUrl}/users", httpMethod = HttpPost, body = $body)
    check response.status == "201 Created"
    let respBody = parseJson(response.body)
    createdUserId = respBody["id"].getInt
    check respBody["email"].getStr == "integration@example.com"

  test "Get Users":
    let client = newHttpClient()
    let response = client.get(fmt"{BaseUrl}/users")
    check response.status == "200 OK"
    let body = parseJson(response.body)
    check body.kind == JArray
    check body.len > 0

  test "Create Task":
    let client = newHttpClient()
    client.headers = newHttpHeaders({ "Content-Type": "application/json" })
    let body = %*{ "title": "Integration Task", "description": "Testing via HTTP" }
    let response = client.request(fmt"{BaseUrl}/users/{createdUserId}/tasks", httpMethod = HttpPost, body = $body)
    check response.status == "201 Created"
    let respBody = parseJson(response.body)
    createdTaskId = respBody["id"].getInt
    check respBody["title"].getStr == "Integration Task"

  test "Get Task":
    let client = newHttpClient()
    let response = client.get(fmt"{BaseUrl}/tasks/{createdTaskId}")
    check response.status == "200 OK"
    let body = parseJson(response.body)
    check body["id"].getInt == createdTaskId

  test "Update Task":
    let client = newHttpClient()
    client.headers = newHttpHeaders({ "Content-Type": "application/json" })
    let body = %*{ "done": true }
    let response = client.request(fmt"{BaseUrl}/tasks/{createdTaskId}", httpMethod = HttpPut, body = $body)
    check response.status == "200 OK"
    let respBody = parseJson(response.body)
    check respBody["done"].getBool == true

  test "Delete Task":
    let client = newHttpClient()
    let response = client.request(fmt"{BaseUrl}/tasks/{createdTaskId}", httpMethod = HttpDelete)
    check response.status == "204 No Content"

  test "Delete User":
    let client = newHttpClient()
    let response = client.request(fmt"{BaseUrl}/users/{createdUserId}", httpMethod = HttpDelete)
    check response.status == "204 No Content"
