#pragma once
#include "../models/User.h"
#include <vector>
#include <optional>

namespace repositories {
    class UserRepository {
    public:
        int create(const models::User& user);
        std::optional<models::User> findById(int id);
        std::vector<models::User> findAll();
        void update(const models::User& user);
        void deleteById(int id);
        bool exists(int id);
    };
}
