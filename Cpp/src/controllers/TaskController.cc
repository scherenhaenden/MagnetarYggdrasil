#include "TaskController.h"

namespace controllers {

    void TaskController::createTask(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback, int id) {
        auto json = req->getJsonObject();
        if (!json || !json->isMember("title") || !json->isMember("description")) {
            auto resp = drogon::HttpResponse::newHttpResponse();
            resp->setStatusCode(drogon::k400BadRequest);
            callback(resp);
            return;
        }

        try {
            auto task = taskService.createTask(id, (*json)["title"].asString(), (*json)["description"].asString());
            if (task) {
                auto resp = drogon::HttpResponse::newHttpJsonResponse(task->toJson());
                resp->setStatusCode(drogon::k201Created);
                callback(resp);
            } else {
                auto resp = drogon::HttpResponse::newHttpResponse();
                resp->setStatusCode(drogon::k404NotFound); // User not found
                callback(resp);
            }
        } catch (const std::exception& e) {
            auto resp = drogon::HttpResponse::newHttpResponse();
            resp->setStatusCode(drogon::k500InternalServerError);
            resp->setBody(e.what());
            callback(resp);
        }
    }

    void TaskController::getTasksByUser(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback, int id) {
        auto tasks = taskService.getTasksByUserId(id);
        if (tasks) {
            Json::Value ret(Json::arrayValue);
            for (const auto& task : *tasks) {
                ret.append(task.toJson());
            }
            auto resp = drogon::HttpResponse::newHttpJsonResponse(ret);
            callback(resp);
        } else {
             auto resp = drogon::HttpResponse::newHttpResponse();
            resp->setStatusCode(drogon::k404NotFound); // User not found
            callback(resp);
        }
    }

    void TaskController::getTaskById(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback, int id) {
        auto task = taskService.getTaskById(id);
        if (task) {
            auto resp = drogon::HttpResponse::newHttpJsonResponse(task->toJson());
            callback(resp);
        } else {
            auto resp = drogon::HttpResponse::newHttpResponse();
            resp->setStatusCode(drogon::k404NotFound);
            callback(resp);
        }
    }

    void TaskController::updateTask(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback, int id) {
        auto json = req->getJsonObject();
        if (!json) {
            auto resp = drogon::HttpResponse::newHttpResponse();
            resp->setStatusCode(drogon::k400BadRequest);
            callback(resp);
            return;
        }

        std::string title = json->isMember("title") ? (*json)["title"].asString() : "";
        std::string description = json->isMember("description") ? (*json)["description"].asString() : "";

        try {
            auto task = taskService.updateTask(id, title, description);
            if (task) {
                auto resp = drogon::HttpResponse::newHttpJsonResponse(task->toJson());
                callback(resp);
            } else {
                auto resp = drogon::HttpResponse::newHttpResponse();
                resp->setStatusCode(drogon::k404NotFound);
                callback(resp);
            }
        } catch (const std::exception& e) {
             auto resp = drogon::HttpResponse::newHttpResponse();
            resp->setStatusCode(drogon::k500InternalServerError);
            resp->setBody(e.what());
            callback(resp);
        }
    }

    void TaskController::markTaskAsDone(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback, int id) {
        if (taskService.markTaskAsDone(id)) {
            auto resp = drogon::HttpResponse::newHttpResponse();
            resp->setStatusCode(drogon::k200OK);
            callback(resp);
        } else {
            auto resp = drogon::HttpResponse::newHttpResponse();
            resp->setStatusCode(drogon::k404NotFound);
            callback(resp);
        }
    }

    void TaskController::deleteTask(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback, int id) {
        if (taskService.deleteTask(id)) {
            auto resp = drogon::HttpResponse::newHttpResponse();
            resp->setStatusCode(drogon::k204NoContent);
            callback(resp);
        } else {
            auto resp = drogon::HttpResponse::newHttpResponse();
            resp->setStatusCode(drogon::k404NotFound);
            callback(resp);
        }
    }
}
