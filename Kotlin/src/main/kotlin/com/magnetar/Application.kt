package com.magnetar

import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import io.ktor.serialization.kotlinx.json.*
import io.ktor.server.application.*
import io.ktor.server.engine.*
import io.ktor.server.netty.*
import io.ktor.server.plugins.contentnegotiation.*
import kotlinx.serialization.json.Json
import org.jetbrains.exposed.sql.Database
import org.jetbrains.exposed.sql.SchemaUtils
import org.jetbrains.exposed.sql.transactions.transaction
import java.io.File

fun main() {
    embeddedServer(Netty, port = 8080, host = "0.0.0.0", module = Application::module)
        .start(wait = true)
}

fun Application.module() {
    install(ContentNegotiation) {
        json(Json {
            prettyPrint = true
            isLenient = true
            ignoreUnknownKeys = true
        })
    }

    initDatabase()

    val userRepository = UserRepository()
    val taskRepository = TaskRepository()
    val userService = UserService(userRepository)
    val taskService = TaskService(taskRepository, userRepository)

    configureRouting(userService, taskService)
}

fun initDatabase() {
    val dbFile = File("magnetar.db")
    // Create DB if it doesn't exist - technically SQLite does this, but good to be explicit
    if (!dbFile.exists()) {
        dbFile.createNewFile()
    }

    val config = HikariConfig().apply {
        jdbcUrl = "jdbc:sqlite:magnetar.db"
        driverClassName = "org.sqlite.JDBC"
        maximumPoolSize = 10
    }
    val dataSource = HikariDataSource(config)
    Database.connect(dataSource)

    transaction {
        SchemaUtils.create(Users, Tasks)
    }
}
