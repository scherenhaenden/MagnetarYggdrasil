#pragma once
#include "../models/Task.h"
#include <vector>
#include <optional>

namespace repositories {
    class TaskRepository {
    public:
        int create(const models::Task& task);
        std::optional<models::Task> findById(int id);
        std::vector<models::Task> findAllByUserId(int userId);
        void update(const models::Task& task);
        void markAsDone(int id);
        void deleteById(int id);
        bool exists(int id);
    };
}
