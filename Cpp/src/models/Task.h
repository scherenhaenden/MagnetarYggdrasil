#pragma once
#include <string>
#include <drogon/drogon.h>

namespace models {
    struct Task {
        int id;
        std::string title;
        std::string description;
        bool is_done;
        int user_id;

        Json::Value toJson() const {
            Json::Value json;
            json["id"] = id;
            json["title"] = title;
            json["description"] = description;
            json["is_done"] = is_done;
            json["user_id"] = user_id;
            return json;
        }
    };
}
