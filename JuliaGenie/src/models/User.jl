module Users

using SearchLight
using SearchLightSQLite

export User

mutable struct User <: AbstractModel
  id::DbId
  username::String
  email::String
end

User() = User(DbId(), "", "")

function SearchLight.schema(::User)
  SearchLight.Schema(
    :users,
    [
      :id => :id,
      :username => :username,
      :email => :email
    ]
  )
end

end
