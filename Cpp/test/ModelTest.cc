#include <gtest/gtest.h>
#include "../src/models/User.h"
#include "../src/models/Task.h"

TEST(ModelTest, UserToJson) {
    models::User user{1, "Alice", "alice@example.com"};
    auto json = user.toJson();
    EXPECT_EQ(json["id"].asInt(), 1);
    EXPECT_EQ(json["name"].asString(), "Alice");
    EXPECT_EQ(json["email"].asString(), "alice@example.com");
}

TEST(ModelTest, TaskToJson) {
    models::Task task{1, "Title", "Desc", false, 2};
    auto json = task.toJson();
    EXPECT_EQ(json["id"].asInt(), 1);
    EXPECT_EQ(json["title"].asString(), "Title");
    EXPECT_EQ(json["description"].asString(), "Desc");
    EXPECT_EQ(json["is_done"].asBool(), false);
    EXPECT_EQ(json["user_id"].asInt(), 2);
}
