class TaskRepository
  def self.create(user_id, params)
    Task.create!(params.merge(user_id: user_id))
  rescue ActiveRecord::RecordInvalid
    nil
  end

  def self.find_by_user_id(user_id)
    Task.where(user_id: user_id)
  end

  def self.find_by_id(id)
    Task.find(id)
  end

  def self.update(id, params)
    task = Task.find(id)
    task.update!(params)
    task
  end

  def self.delete(id)
    task = Task.find(id)
    task.destroy
  end
end
