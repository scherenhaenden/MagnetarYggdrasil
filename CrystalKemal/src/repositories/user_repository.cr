require "db"
require "../models/user"
require "../db"

module App
  class UserRepository
    def self.create(username : String, email : String) : User
      db = Database.connection
      begin
        result = db.exec "INSERT INTO users (username, email) VALUES (?, ?)", username, email
        id = result.last_insert_id
        User.new(username, email, id)
      ensure
        db.close
      end
    end

    def self.find_all : Array(User)
      db = Database.connection
      begin
        users = [] of User
        # Corrected order: Select matches read order or read into vars
        db.query "SELECT username, email, id FROM users" do |rs|
          rs.each do
            users << User.new(rs.read(String), rs.read(String), rs.read(Int64))
          end
        end
        users
      ensure
        db.close
      end
    end

    def self.find_by_id(id : Int64) : User?
      db = Database.connection
      begin
        # Corrected order
        db.query_one? "SELECT username, email, id FROM users WHERE id = ?", id do |rs|
          User.new(rs.read(String), rs.read(String), rs.read(Int64))
        end
      ensure
        db.close
      end
    end

    def self.update(id : Int64, username : String) : User?
      db = Database.connection
      begin
        result = db.exec "UPDATE users SET username = ? WHERE id = ?", username, id
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
        result = db.exec "DELETE FROM users WHERE id = ?", id
        result.rows_affected > 0
      ensure
        db.close
      end
    end
  end
end
