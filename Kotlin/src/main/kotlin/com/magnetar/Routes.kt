package com.magnetar

import io.ktor.http.*
import io.ktor.server.application.*
import io.ktor.server.request.*
import io.ktor.server.response.*
import io.ktor.server.routing.*

fun Application.configureRouting(userService: UserService, taskService: TaskService) {
    routing {
        get("/health") {
            call.respond(HttpStatusCode.OK, mapOf("status" to "ok"))
        }

        route("/users") {
            post {
                try {
                    val request = call.receive<CreateUserRequest>()
                    val user = userService.create(request)
                    call.respond(HttpStatusCode.Created, user)
                } catch (e: Exception) {
                    call.respond(HttpStatusCode.BadRequest, mapOf("error" to e.message))
                }
            }

            get {
                val users = userService.findAll()
                call.respond(users)
            }

            get("/{id}") {
                val id = call.parameters["id"]?.toIntOrNull()
                if (id == null) {
                    call.respond(HttpStatusCode.BadRequest, mapOf("error" to "Invalid ID"))
                    return@get
                }
                val user = userService.findById(id)
                if (user == null) {
                    call.respond(HttpStatusCode.NotFound)
                } else {
                    call.respond(user)
                }
            }

            put("/{id}") {
                val id = call.parameters["id"]?.toIntOrNull()
                if (id == null) {
                    call.respond(HttpStatusCode.BadRequest, mapOf("error" to "Invalid ID"))
                    return@put
                }
                val request = call.receive<UpdateUserRequest>()
                val user = userService.update(id, request)
                if (user == null) {
                    call.respond(HttpStatusCode.NotFound)
                } else {
                    call.respond(user)
                }
            }

            delete("/{id}") {
                val id = call.parameters["id"]?.toIntOrNull()
                if (id == null) {
                    call.respond(HttpStatusCode.BadRequest, mapOf("error" to "Invalid ID"))
                    return@delete
                }
                if (userService.delete(id)) {
                    call.respond(HttpStatusCode.NoContent)
                } else {
                    call.respond(HttpStatusCode.NotFound)
                }
            }

            route("/{id}/tasks") {
                post {
                    val id = call.parameters["id"]?.toIntOrNull()
                    if (id == null) {
                        call.respond(HttpStatusCode.BadRequest, mapOf("error" to "Invalid ID"))
                        return@post
                    }
                    try {
                        val request = call.receive<CreateTaskRequest>()
                        val task = taskService.create(id, request)
                        call.respond(HttpStatusCode.Created, task)
                    } catch (e: IllegalArgumentException) {
                        call.respond(HttpStatusCode.BadRequest, mapOf("error" to e.message))
                    } catch (e: Exception) {
                         call.respond(HttpStatusCode.InternalServerError, mapOf("error" to e.message))
                    }
                }

                get {
                    val id = call.parameters["id"]?.toIntOrNull()
                    if (id == null) {
                        call.respond(HttpStatusCode.BadRequest, mapOf("error" to "Invalid ID"))
                        return@get
                    }
                    try {
                        val tasks = taskService.findByUserId(id)
                        call.respond(tasks)
                    } catch (e: IllegalArgumentException) {
                        call.respond(HttpStatusCode.NotFound, mapOf("error" to e.message))
                    }
                }
            }
        }

        route("/tasks") {
            get("/{tid}") {
                val tid = call.parameters["tid"]?.toIntOrNull()
                if (tid == null) {
                    call.respond(HttpStatusCode.BadRequest, mapOf("error" to "Invalid Task ID"))
                    return@get
                }
                val task = taskService.findById(tid)
                if (task == null) {
                    call.respond(HttpStatusCode.NotFound)
                } else {
                    call.respond(task)
                }
            }

            put("/{tid}") {
                val tid = call.parameters["tid"]?.toIntOrNull()
                if (tid == null) {
                    call.respond(HttpStatusCode.BadRequest, mapOf("error" to "Invalid Task ID"))
                    return@put
                }
                val request = call.receive<UpdateTaskRequest>()
                val task = taskService.update(tid, request)
                if (task == null) {
                    call.respond(HttpStatusCode.NotFound)
                } else {
                    call.respond(task)
                }
            }

            patch("/{tid}/done") {
                val tid = call.parameters["tid"]?.toIntOrNull()
                if (tid == null) {
                    call.respond(HttpStatusCode.BadRequest, mapOf("error" to "Invalid Task ID"))
                    return@patch
                }
                val task = taskService.markDone(tid)
                if (task == null) {
                    call.respond(HttpStatusCode.NotFound)
                } else {
                    call.respond(task)
                }
            }

            delete("/{tid}") {
                val tid = call.parameters["tid"]?.toIntOrNull()
                if (tid == null) {
                    call.respond(HttpStatusCode.BadRequest, mapOf("error" to "Invalid Task ID"))
                    return@delete
                }
                if (taskService.delete(tid)) {
                    call.respond(HttpStatusCode.NoContent)
                } else {
                    call.respond(HttpStatusCode.NotFound)
                }
            }
        }
    }
}
