module UsersRepository

using SearchLight
using ..Users: User

function create(user::User)::User
  save!(user)
end

function all()::Vector{User}
  SearchLight.all(User)
end

function find(id::Int)::Union{User, Nothing}
  SearchLight.find_one(User, id)
end

function find_by_username(username::String)::Union{User, Nothing}
  SearchLight.find_one(User, username = username)
end

function find_by_email(email::String)::Union{User, Nothing}
  SearchLight.find_one(User, email = email)
end

function update(user::User)::User
  save!(user)
end

function delete(id::Int)
  u = find(id)
  if u !== nothing
    SearchLight.delete(u)
  end
end

end
