require "../repositories/user_repository"

module App
  class UserService
    def self.create_user(username : String, email : String) : User
      # Validation logic could go here
      UserRepository.create(username, email)
    end

    def self.get_all_users : Array(User)
      UserRepository.find_all
    end

    def self.get_user(id : Int64) : User?
      UserRepository.find_by_id(id)
    end

    def self.update_user(id : Int64, username : String) : User?
      UserRepository.update(id, username)
    end

    def self.delete_user(id : Int64) : Bool
      UserRepository.delete(id)
    end
  end
end
