class UserRepository
  def self.create(params)
    User.create!(params)
  rescue ActiveRecord::RecordInvalid => e
    nil
  end

  def self.find_all
    User.all
  end

  def self.find_by_id(id)
    User.find(id)
  end

  def self.update(id, params)
    user = User.find(id)
    user.update!(params)
    user
  end

  def self.delete(id)
    user = User.find(id)
    user.destroy
  end
end
