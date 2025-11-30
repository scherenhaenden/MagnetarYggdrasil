#pragma once
#include <drogon/HttpController.h>
#include "../services/TaskService.h"

namespace controllers {
    class TaskController : public drogon::HttpController<TaskController> {
    public:
        METHOD_LIST_BEGIN
        ADD_METHOD_TO(TaskController::createTask, "/users/{id}/tasks", drogon::Post);
        ADD_METHOD_TO(TaskController::getTasksByUser, "/users/{id}/tasks", drogon::Get);
        ADD_METHOD_TO(TaskController::getTaskById, "/tasks/{id}", drogon::Get);
        ADD_METHOD_TO(TaskController::updateTask, "/tasks/{id}", drogon::Put);
        ADD_METHOD_TO(TaskController::markTaskAsDone, "/tasks/{id}/done", drogon::Patch);
        ADD_METHOD_TO(TaskController::deleteTask, "/tasks/{id}", drogon::Delete);
        METHOD_LIST_END

        void createTask(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback, int id);
        void getTasksByUser(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback, int id);
        void getTaskById(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback, int id);
        void updateTask(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback, int id);
        void markTaskAsDone(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback, int id);
        void deleteTask(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback, int id);

    private:
        services::TaskService taskService;
    };
}
