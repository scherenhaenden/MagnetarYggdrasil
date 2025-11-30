require "../repositories/task_repository"

module App
  class TaskService
    def self.create_task(user_id : Int64, title : String, description : String) : Task
      TaskRepository.create(user_id, title, description)
    end

    def self.get_tasks_for_user(user_id : Int64) : Array(Task)
      TaskRepository.find_all_by_user(user_id)
    end

    def self.get_task(id : Int64) : Task?
      TaskRepository.find_by_id(id)
    end

    def self.update_task(id : Int64, title : String, description : String) : Task?
      TaskRepository.update(id, title, description)
    end

    def self.mark_task_done(id : Int64) : Task?
      TaskRepository.mark_done(id)
    end

    def self.delete_task(id : Int64) : Bool
      TaskRepository.delete(id)
    end
  end
end
