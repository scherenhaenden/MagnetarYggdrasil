class TaskService
  def self.create(user_id, params)
    # Validate user exists
    UserRepository.find_by_id(user_id)
    TaskRepository.create(user_id, params)
  end

  def self.list_by_user(user_id)
    # Validate user exists
    UserRepository.find_by_id(user_id)
    TaskRepository.find_by_user_id(user_id)
  end

  def self.get(id)
    TaskRepository.find_by_id(id)
  end

  def self.update(id, params)
    TaskRepository.update(id, params)
  end

  def self.mark_done(id)
    TaskRepository.update(id, { done: true })
  end

  def self.delete(id)
    TaskRepository.delete(id)
  end
end
