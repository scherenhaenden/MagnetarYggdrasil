#include <drogon/drogon.h>
#include "db/Database.h"

int main() {
    db::Database::getInstance().init();
    drogon::app().loadConfigFile("config.json").run();
    return 0;
}
