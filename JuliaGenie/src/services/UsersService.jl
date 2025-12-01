module UsersService

using ..UsersRepository
using ..Users: User

struct UserDTO
  id::Union{Int, Nothing}
  username::String
  email::String
end

function create_user(username::String, email::String)::UserDTO
  # Validation
  if isempty(username) || isempty(email)
      error("Username and email are required")
  end

  if UsersRepository.find_by_username(username) !== nothing
      error("Username already exists")
  end

  if UsersRepository.find_by_email(email) !== nothing
      error("Email already exists")
  end

  user = User()
  user.username = username
  user.email = email

  saved_user = UsersRepository.create(user)
  UserDTO(Base.convert(Int, saved_user.id), saved_user.username, saved_user.email)
end

function list_users()::Vector{UserDTO}
  users = UsersRepository.all()
  map(u -> UserDTO(Base.convert(Int, u.id), u.username, u.email), users)
end

function get_user(id::Int)::Union{UserDTO, Nothing}
  u = UsersRepository.find(id)
  if u === nothing
    return nothing
  end
  UserDTO(Base.convert(Int, u.id), u.username, u.email)
end

function update_user(id::Int, username::String)::Union{UserDTO, Nothing}
  u = UsersRepository.find(id)
  if u === nothing
    return nothing
  end

  # Validation
  if isempty(username)
      error("Username cannot be empty")
  end

  existing = UsersRepository.find_by_username(username)
  if existing !== nothing && Base.convert(Int, existing.id) != id
      error("Username already taken")
  end

  u.username = username
  saved_user = UsersRepository.update(u)
  UserDTO(Base.convert(Int, saved_user.id), saved_user.username, saved_user.email)
end

function delete_user(id::Int)
  UsersRepository.delete(id)
end

end
