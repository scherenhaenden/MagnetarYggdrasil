#pragma once
#include <string>
#include <drogon/drogon.h>

namespace models {
    struct User {
        int id;
        std::string name;
        std::string email;

        Json::Value toJson() const {
            Json::Value json;
            json["id"] = id;
            json["name"] = name;
            json["email"] = email;
            return json;
        }
    };
}
