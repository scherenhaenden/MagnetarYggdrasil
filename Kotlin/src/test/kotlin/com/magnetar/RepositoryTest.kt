package com.magnetar

import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import org.jetbrains.exposed.sql.Database
import org.jetbrains.exposed.sql.SchemaUtils
import org.jetbrains.exposed.sql.transactions.transaction
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test

class RepositoryTest {
    private lateinit var dataSource: HikariDataSource
    private lateinit var userRepository: UserRepository
    private lateinit var taskRepository: TaskRepository

    @BeforeEach
    fun setup() {
        val config = HikariConfig().apply {
            jdbcUrl = "jdbc:sqlite::memory:"
            driverClassName = "org.sqlite.JDBC"
            maximumPoolSize = 1
        }
        dataSource = HikariDataSource(config)
        Database.connect(dataSource)

        transaction {
            SchemaUtils.create(Users, Tasks)
        }

        userRepository = UserRepository()
        taskRepository = TaskRepository()
    }

    @AfterEach
    fun tearDown() {
        dataSource.close()
    }

    @Test
    fun `should create and retrieve user`() {
        val user = userRepository.create(CreateUserRequest("Bob", "bob@example.com"))
        assertNotNull(user.id)
        assertEquals("Bob", user.name)

        val retrieved = userRepository.findById(user.id)
        assertEquals(user, retrieved)
    }

    @Test
    fun `should update user`() {
        val user = userRepository.create(CreateUserRequest("Bob", "bob@example.com"))
        val updated = userRepository.update(user.id, UpdateUserRequest(name = "Bobby"))

        assertEquals("Bobby", updated?.name)
        assertEquals("bob@example.com", updated?.email)
    }

    @Test
    fun `should delete user`() {
        val user = userRepository.create(CreateUserRequest("Bob", "bob@example.com"))
        val result = userRepository.delete(user.id)
        assertTrue(result)
        assertNull(userRepository.findById(user.id))
    }

    @Test
    fun `should create and retrieve task`() {
        val user = userRepository.create(CreateUserRequest("Bob", "bob@example.com"))
        val task = taskRepository.create(user.id, CreateTaskRequest("Fix bugs", "Urgent"))

        assertNotNull(task.id)
        assertEquals(user.id, task.userId)

        val retrieved = taskRepository.findById(task.id)
        assertEquals(task, retrieved)
    }

    @Test
    fun `should update task`() {
        val user = userRepository.create(CreateUserRequest("Bob", "bob@example.com"))
        val task = taskRepository.create(user.id, CreateTaskRequest("Fix bugs", "Urgent"))

        val updated = taskRepository.update(task.id, UpdateTaskRequest(isDone = true))
        assertTrue(updated?.isDone == true)
    }
}
