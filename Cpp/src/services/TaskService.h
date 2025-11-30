#pragma once
#include "../models/Task.h"
#include "../repositories/TaskRepository.h"
#include "../repositories/UserRepository.h"
#include <vector>
#include <optional>

namespace services {
    class TaskService {
    public:
        TaskService();
        std::optional<models::Task> createTask(int userId, const std::string& title, const std::string& description);
        std::optional<models::Task> getTaskById(int id);
        std::optional<std::vector<models::Task>> getTasksByUserId(int userId);
        std::optional<models::Task> updateTask(int id, const std::string& title, const std::string& description);
        bool markTaskAsDone(int id);
        bool deleteTask(int id);

    private:
        repositories::TaskRepository taskRepo;
        repositories::UserRepository userRepo;
    };
}
