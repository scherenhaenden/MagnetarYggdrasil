#pragma once
#include <drogon/HttpController.h>
#include "../services/UserService.h"

namespace controllers {
    class UserController : public drogon::HttpController<UserController> {
    public:
        METHOD_LIST_BEGIN
        ADD_METHOD_TO(UserController::getUsers, "/users", drogon::Get);
        ADD_METHOD_TO(UserController::createUser, "/users", drogon::Post);
        ADD_METHOD_TO(UserController::getUserById, "/users/{id}", drogon::Get);
        ADD_METHOD_TO(UserController::updateUser, "/users/{id}", drogon::Put);
        ADD_METHOD_TO(UserController::deleteUser, "/users/{id}", drogon::Delete);
        METHOD_LIST_END

        void getUsers(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback);
        void createUser(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback);
        void getUserById(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback, int id);
        void updateUser(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback, int id);
        void deleteUser(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback, int id);

    private:
        services::UserService userService;
    };
}
