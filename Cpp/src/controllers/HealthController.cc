#include "HealthController.h"

namespace controllers {
    void HealthController::health(const drogon::HttpRequestPtr& req, std::function<void(const drogon::HttpResponsePtr&)>&& callback) {
        Json::Value ret;
        ret["status"] = "ok";
        auto resp = drogon::HttpResponse::newHttpJsonResponse(ret);
        callback(resp);
    }
}
