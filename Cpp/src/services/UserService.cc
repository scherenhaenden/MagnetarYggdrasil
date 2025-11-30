#include "UserService.h"
#include <iostream>

namespace services {

    UserService::UserService() {}

    models::User UserService::createUser(const std::string& name, const std::string& email) {
        models::User user;
        user.name = name;
        user.email = email;
        int id = repo.create(user);
        user.id = id;
        return user;
    }

    std::optional<models::User> UserService::getUserById(int id) {
        return repo.findById(id);
    }

    std::vector<models::User> UserService::getAllUsers() {
        return repo.findAll();
    }

    std::optional<models::User> UserService::updateUser(int id, const std::string& name, const std::string& email) {
        auto user = repo.findById(id);
        if (!user) return std::nullopt;

        if (!name.empty()) user->name = name;
        if (!email.empty()) user->email = email;

        repo.update(*user);
        return user;
    }

    bool UserService::deleteUser(int id) {
        if (!repo.exists(id)) return false;
        repo.deleteById(id);
        return true;
    }
}
