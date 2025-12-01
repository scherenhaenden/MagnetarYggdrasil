require "kemal"
require "../services/task_service"

module App
  class TaskController
    def self.register
      post "/users/:id/tasks" do |env|
        begin
          user_id = env.params.url["id"].to_i64
          title = env.params.json["title"].as(String)
          description = env.params.json["description"].as(String)

          task = TaskService.create_task(user_id, title, description)
          env.response.status_code = 201
          task.to_json
        rescue ex
          env.response.status_code = 400
          {error: "Invalid input or User not found"}.to_json
        end
      end

      get "/users/:id/tasks" do |env|
        begin
          user_id = env.params.url["id"].to_i64
          tasks = TaskService.get_tasks_for_user(user_id)
          tasks.to_json
        rescue
          env.response.status_code = 400
          {error: "Invalid User ID"}.to_json
        end
      end

      get "/tasks/:id" do |env|
        begin
          id = env.params.url["id"].to_i64
          task = TaskService.get_task(id)
          if task
            task.to_json
          else
            env.response.status_code = 404
            {error: "Task not found"}.to_json
          end
        rescue
          env.response.status_code = 400
          {error: "Invalid ID"}.to_json
        end
      end

      put "/tasks/:id" do |env|
        begin
          id = env.params.url["id"].to_i64
          title = env.params.json["title"].as(String)
          description = env.params.json["description"].as(String)
          task = TaskService.update_task(id, title, description)
          if task
            task.to_json
          else
            env.response.status_code = 404
            {error: "Task not found"}.to_json
          end
        rescue ex
          env.response.status_code = 400
          {error: "Invalid input"}.to_json
        end
      end

      patch "/tasks/:id/done" do |env|
        begin
          id = env.params.url["id"].to_i64
          task = TaskService.mark_task_done(id)
          if task
            task.to_json
          else
            env.response.status_code = 404
            {error: "Task not found"}.to_json
          end
        rescue
          env.response.status_code = 400
          {error: "Invalid ID"}.to_json
        end
      end

      delete "/tasks/:id" do |env|
        begin
          id = env.params.url["id"].to_i64
          success = TaskService.delete_task(id)
          if success
            env.response.status_code = 204
            ""
          else
            env.response.status_code = 404
            {error: "Task not found"}.to_json
          end
        rescue
           env.response.status_code = 400
           {error: "Invalid ID"}.to_json
        end
      end
    end
  end
end
