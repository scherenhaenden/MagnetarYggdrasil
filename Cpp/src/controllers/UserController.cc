#include "UserController.h"

namespace controllers {

    void UserController::getUsers(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback) {
        auto users = userService.getAllUsers();
        Json::Value ret(Json::arrayValue);
        for (const auto& user : users) {
            ret.append(user.toJson());
        }
        auto resp = drogon::HttpResponse::newHttpJsonResponse(ret);
        callback(resp);
    }

    void UserController::createUser(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback) {
        auto json = req->getJsonObject();
        if (!json || !json->isMember("name") || !json->isMember("email")) {
            auto resp = drogon::HttpResponse::newHttpResponse();
            resp->setStatusCode(drogon::k400BadRequest);
            callback(resp);
            return;
        }

        try {
            auto user = userService.createUser((*json)["name"].asString(), (*json)["email"].asString());
            auto resp = drogon::HttpResponse::newHttpJsonResponse(user.toJson());
            resp->setStatusCode(drogon::k201Created);
            callback(resp);
        } catch (const std::exception& e) {
            auto resp = drogon::HttpResponse::newHttpResponse();
            resp->setStatusCode(drogon::k400BadRequest); // Likely duplicate email
            resp->setBody(e.what());
            callback(resp);
        }
    }

    void UserController::getUserById(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback, int id) {
        auto user = userService.getUserById(id);
        if (user) {
            auto resp = drogon::HttpResponse::newHttpJsonResponse(user->toJson());
            callback(resp);
        } else {
            auto resp = drogon::HttpResponse::newHttpResponse();
            resp->setStatusCode(drogon::k404NotFound);
            callback(resp);
        }
    }

    void UserController::updateUser(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback, int id) {
        auto json = req->getJsonObject();
        if (!json) {
            auto resp = drogon::HttpResponse::newHttpResponse();
            resp->setStatusCode(drogon::k400BadRequest);
            callback(resp);
            return;
        }

        std::string name = json->isMember("name") ? (*json)["name"].asString() : "";
        std::string email = json->isMember("email") ? (*json)["email"].asString() : "";

        try {
            auto user = userService.updateUser(id, name, email);
            if (user) {
                auto resp = drogon::HttpResponse::newHttpJsonResponse(user->toJson());
                callback(resp);
            } else {
                auto resp = drogon::HttpResponse::newHttpResponse();
                resp->setStatusCode(drogon::k404NotFound);
                callback(resp);
            }
        } catch (const std::exception& e) {
             auto resp = drogon::HttpResponse::newHttpResponse();
            resp->setStatusCode(drogon::k400BadRequest);
            resp->setBody(e.what());
            callback(resp);
        }
    }

    void UserController::deleteUser(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback, int id) {
        if (userService.deleteUser(id)) {
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
