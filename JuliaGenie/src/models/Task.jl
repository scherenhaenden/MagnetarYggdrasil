module Tasks

using SearchLight
using SearchLightSQLite

export Task

mutable struct Task <: AbstractModel
  id::DbId
  user_id::DbId
  title::String
  description::String
  done::Bool
end

Task() = Task(DbId(), DbId(), "", "", false)

function SearchLight.schema(::Task)
  SearchLight.Schema(
    :tasks,
    [
      :id => :id,
      :user_id => :user_id,
      :title => :title,
      :description => :description,
      :done => :done
    ]
  )
end

end
