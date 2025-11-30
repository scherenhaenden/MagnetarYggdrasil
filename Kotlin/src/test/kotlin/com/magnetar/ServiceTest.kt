package com.magnetar

import io.mockk.every
import io.mockk.mockk
import io.mockk.verify
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import kotlin.test.assertFailsWith

class UserServiceTest {

    private val userRepository = mockk<UserRepository>()
    private val userService = UserService(userRepository)

    @Test
    fun `create should create user when input is valid`() {
        val request = CreateUserRequest("Alice", "alice@example.com")
        val expectedUser = User(1, "Alice", "alice@example.com")

        every { userRepository.create(request) } returns expectedUser

        val result = userService.create(request)

        assertEquals(expectedUser, result)
        verify { userRepository.create(request) }
    }

    @Test
    fun `create should throw exception when name is blank`() {
        val request = CreateUserRequest("", "alice@example.com")

        assertFailsWith<IllegalArgumentException> {
            userService.create(request)
        }
    }

    @Test
    fun `findAll should return all users`() {
        val users = listOf(User(1, "Alice", "alice@example.com"))
        every { userRepository.findAll() } returns users

        val result = userService.findAll()

        assertEquals(users, result)
    }
}

class TaskServiceTest {

    private val taskRepository = mockk<TaskRepository>()
    private val userRepository = mockk<UserRepository>()
    private val taskService = TaskService(taskRepository, userRepository)

    @Test
    fun `create should create task when user exists and input is valid`() {
        val userId = 1
        val request = CreateTaskRequest("Task 1", "Description")
        val expectedTask = Task(1, userId, "Task 1", "Description", false)

        every { userRepository.findById(userId) } returns User(userId, "Alice", "alice@example.com")
        every { taskRepository.create(userId, request) } returns expectedTask

        val result = taskService.create(userId, request)

        assertEquals(expectedTask, result)
    }

    @Test
    fun `create should throw exception when user does not exist`() {
        val userId = 1
        val request = CreateTaskRequest("Task 1", "Description")

        every { userRepository.findById(userId) } returns null

        assertFailsWith<IllegalArgumentException> {
            taskService.create(userId, request)
        }
    }
}
