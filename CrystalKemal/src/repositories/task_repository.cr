require "db"
require "../models/task"
require "../db"

module App
  class TaskRepository
    def self.create(user_id : Int64, title : String, description : String) : Task
      db = Database.connection
      begin
        result = db.exec "INSERT INTO tasks (user_id, title, description, done) VALUES (?, ?, ?, 0)", user_id, title, description
        id = result.last_insert_id
        Task.new(user_id, title, description, false, id)
      ensure
        db.close
      end
    end

    def self.find_all_by_user(user_id : Int64) : Array(Task)
      db = Database.connection
      begin
        tasks = [] of Task
        db.query "SELECT id, user_id, title, description, done FROM tasks WHERE user_id = ?", user_id do |rs|
          rs.each do
            id = rs.read(Int64)
            u_id = rs.read(Int64)
            title = rs.read(String)
            desc = rs.read(String)
            done_int = rs.read(Int64)
            tasks << Task.new(u_id, title, desc, done_int == 1, id)
          end
        end
        tasks
      ensure
        db.close
      end
    end

    def self.find_by_id(id : Int64) : Task?
      db = Database.connection
      begin
        db.query_one? "SELECT id, user_id, title, description, done FROM tasks WHERE id = ?", id do |rs|
          id_val = rs.read(Int64)
          u_id = rs.read(Int64)
          title = rs.read(String)
          desc = rs.read(String)
          done_int = rs.read(Int64)
          Task.new(u_id, title, desc, done_int == 1, id_val)
        end
      ensure
        db.close
      end
    end

    def self.update(id : Int64, title : String, description : String) : Task?
      db = Database.connection
      begin
        result = db.exec "UPDATE tasks SET title = ?, description = ? WHERE id = ?", title, description, id
        if result.rows_affected > 0
          find_by_id(id)
        else
          nil
        end
      ensure
        db.close
      end
    end

    def self.mark_done(id : Int64) : Task?
      db = Database.connection
      begin
        result = db.exec "UPDATE tasks SET done = 1 WHERE id = ?", id
        if result.rows_affected > 0
          find_by_id(id)
        else
          nil
        end
      ensure
        db.close
      end
    end

    def self.delete(id : Int64) : Bool
      db = Database.connection
      begin
        result = db.exec "DELETE FROM tasks WHERE id = ?", id
        result.rows_affected > 0
      ensure
        db.close
      end
    end
  end
end
