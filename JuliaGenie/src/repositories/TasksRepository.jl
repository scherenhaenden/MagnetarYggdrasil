module TasksRepository

using SearchLight
using ..Tasks: Task

function create(task::Task)::Task
  save!(task)
end

function all_for_user(user_id::Int)::Vector{Task}
  find(Task, user_id = user_id)
end

function find(id::Int)::Union{Task, Nothing}
  SearchLight.find_one(Task, id)
end

function update(task::Task)::Task
  save!(task)
end

function delete(id::Int)
  t = find(id)
  if t !== nothing
    SearchLight.delete(t)
  end
end

end
