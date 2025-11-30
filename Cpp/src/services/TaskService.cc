#include "TaskService.h"

namespace services {

    TaskService::TaskService() {}

    std::optional<models::Task> TaskService::createTask(int userId, const std::string& title, const std::string& description) {
        if (!userRepo.exists(userId)) return std::nullopt;

        models::Task task;
        task.title = title;
        task.description = description;
        task.user_id = userId;
        task.is_done = false;

        int id = taskRepo.create(task);
        task.id = id;
        return task;
    }

    std::optional<models::Task> TaskService::getTaskById(int id) {
        return taskRepo.findById(id);
    }

    std::optional<std::vector<models::Task>> TaskService::getTasksByUserId(int userId) {
        if (!userRepo.exists(userId)) return std::nullopt;
        return taskRepo.findAllByUserId(userId);
    }

    std::optional<models::Task> TaskService::updateTask(int id, const std::string& title, const std::string& description) {
        auto task = taskRepo.findById(id);
        if (!task) return std::nullopt;

        if (!title.empty()) task->title = title;
        if (!description.empty()) task->description = description;

        taskRepo.update(*task);
        return task;
    }

    bool TaskService::markTaskAsDone(int id) {
        if (!taskRepo.exists(id)) return false;
        taskRepo.markAsDone(id);
        return true;
    }

    bool TaskService::deleteTask(int id) {
        if (!taskRepo.exists(id)) return false;
        taskRepo.deleteById(id);
        return true;
    }
}
