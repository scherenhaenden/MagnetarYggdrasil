package com.magnetar

class UserService(private val repository: UserRepository) {
    fun create(user: CreateUserRequest): User {
        // Business logic could go here, e.g. validation
        if (user.name.isBlank()) throw IllegalArgumentException("Name cannot be blank")
        if (user.email.isBlank()) throw IllegalArgumentException("Email cannot be blank")
        return repository.create(user)
    }

    fun findAll(): List<User> = repository.findAll()

    fun findById(id: Int): User? = repository.findById(id)

    fun update(id: Int, update: UpdateUserRequest): User? = repository.update(id, update)

    fun delete(id: Int): Boolean = repository.delete(id)
}

class TaskService(private val repository: TaskRepository, private val userRepository: UserRepository) {
    fun create(userId: Int, task: CreateTaskRequest): Task {
        if (userRepository.findById(userId) == null) {
            throw IllegalArgumentException("User not found")
        }
        if (task.title.isBlank()) throw IllegalArgumentException("Title cannot be blank")
        return repository.create(userId, task)
    }

    fun findByUserId(userId: Int): List<Task> {
        if (userRepository.findById(userId) == null) {
            throw IllegalArgumentException("User not found")
        }
        return repository.findByUserId(userId)
    }

    fun findById(id: Int): Task? = repository.findById(id)

    fun update(id: Int, update: UpdateTaskRequest): Task? = repository.update(id, update)

    fun markDone(id: Int): Task? = repository.markDone(id)

    fun delete(id: Int): Boolean = repository.delete(id)
}
