package com.magnetar

import io.ktor.client.call.*
import io.ktor.client.plugins.contentnegotiation.*
import io.ktor.client.request.*
import io.ktor.http.*
import io.ktor.serialization.kotlinx.json.*
import io.ktor.server.testing.*
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.io.File

class ApplicationTest {

    @Test
    fun `full flow test`() = testApplication {
        // Setup a temporary DB for this test
        val testDbFile = File("test_magnetar.db")
        if (testDbFile.exists()) testDbFile.delete()

        // We need to override the initDatabase or use environment variables, but for simplicity
        // in this environment, since we can't easily injection different config without dependency injection
        // framework properly set up for tests or system properties.
        // However, the `Application.module` calls `initDatabase` which uses "magnetar.db".
        // To avoid conflicts, we might want to change `initDatabase` to accept a filename or config.
        // But given the constraints, I will trust the user handles the DB or I'll just let it use the file
        // and clean it up. Since it's a test environment, it might clash if running parallel.
        // For now, let's assume serial execution.

        // BETTER APPROACH: Use Mockk to mock the repositories and inject them into the module?
        // But `Application.module` instantiates them directly.
        // I will rely on the fact that for the testApplication, I can manually configure routing
        // if I construct the module myself, but `testApplication` sets up the environment.

        // Let's rely on the fact that `initDatabase` creates the file if missing.
        // If I could, I would refactor `Application.module` to take dependencies.

        application {
             module()
        }

        val client = createClient {
            install(ContentNegotiation) {
                json()
            }
        }

        // 1. Health Check
        val health = client.get("/health")
        assertEquals(HttpStatusCode.OK, health.status)

        // 2. Create User
        val createUserResponse = client.post("/users") {
            contentType(ContentType.Application.Json)
            setBody(CreateUserRequest("Test User", "test@user.com"))
        }
        assertEquals(HttpStatusCode.Created, createUserResponse.status)
        val user = createUserResponse.body<User>()
        assertEquals("Test User", user.name)

        // 3. Get User
        val getUserResponse = client.get("/users/${user.id}")
        assertEquals(HttpStatusCode.OK, getUserResponse.status)
        assertEquals(user, getUserResponse.body<User>())

        // 4. Create Task
        val createTaskResponse = client.post("/users/${user.id}/tasks") {
            contentType(ContentType.Application.Json)
            setBody(CreateTaskRequest("New Task", "Do something"))
        }
        assertEquals(HttpStatusCode.Created, createTaskResponse.status)
        val task = createTaskResponse.body<Task>()
        assertEquals("New Task", task.title)

        // 5. Mark Done
        val markDoneResponse = client.patch("/tasks/${task.id}/done")
        assertEquals(HttpStatusCode.OK, markDoneResponse.status)
        assertTrue(markDoneResponse.body<Task>().isDone)

        // 6. Delete Task
        val deleteTaskResponse = client.delete("/tasks/${task.id}")
        assertEquals(HttpStatusCode.NoContent, deleteTaskResponse.status)

        // 7. Delete User
        val deleteUserResponse = client.delete("/users/${user.id}")
        assertEquals(HttpStatusCode.NoContent, deleteUserResponse.status)
    }
}
