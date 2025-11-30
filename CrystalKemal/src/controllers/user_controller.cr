require "kemal"
require "../services/user_service"

module App
  class UserController
    def self.register
      post "/users" do |env|
        begin
          username = env.params.json["username"].as(String)
          email = env.params.json["email"].as(String)
          user = UserService.create_user(username, email)
          env.response.status_code = 201
          user.to_json
        rescue ex
          env.response.status_code = 400
          {error: "Invalid input"}.to_json
        end
      end

      get "/users" do |env|
        users = UserService.get_all_users
        users.to_json
      end

      get "/users/:id" do |env|
        begin
          id = env.params.url["id"].to_i64
          user = UserService.get_user(id)
          if user
            user.to_json
          else
            env.response.status_code = 404
            {error: "User not found"}.to_json
          end
        rescue
          env.response.status_code = 400
          {error: "Invalid ID"}.to_json
        end
      end

      put "/users/:id" do |env|
        begin
          id = env.params.url["id"].to_i64
          username = env.params.json["username"].as(String)
          user = UserService.update_user(id, username)
          if user
            user.to_json
          else
            env.response.status_code = 404
            {error: "User not found"}.to_json
          end
        rescue ex
          env.response.status_code = 400
          {error: "Invalid input"}.to_json
        end
      end

      delete "/users/:id" do |env|
        begin
          id = env.params.url["id"].to_i64
          success = UserService.delete_user(id)
          if success
            env.response.status_code = 204
            ""
          else
            env.response.status_code = 404
            {error: "User not found"}.to_json
          end
        rescue
           env.response.status_code = 400
           {error: "Invalid ID"}.to_json
        end
      end
    end
  end
end
