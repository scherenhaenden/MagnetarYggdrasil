#pragma once
#include "../models/User.h"
#include "../repositories/UserRepository.h"
#include <vector>
#include <optional>

namespace services {
    class UserService {
    public:
        UserService();
        models::User createUser(const std::string& name, const std::string& email);
        std::optional<models::User> getUserById(int id);
        std::vector<models::User> getAllUsers();
        std::optional<models::User> updateUser(int id, const std::string& name, const std::string& email);
        bool deleteUser(int id);

    private:
        repositories::UserRepository repo;
    };
}
