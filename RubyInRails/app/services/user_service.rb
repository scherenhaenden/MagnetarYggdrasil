class UserService
  def self.create(params)
    UserRepository.create(params)
  end

  def self.list_all
    UserRepository.find_all
  end

  def self.get(id)
    UserRepository.find_by_id(id)
  end

  def self.update(id, params)
    UserRepository.update(id, params)
  end

  def self.delete(id)
    UserRepository.delete(id)
  end
end
