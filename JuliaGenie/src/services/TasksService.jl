module TasksService

using ..TasksRepository
using ..UsersRepository
using ..Tasks: Task

struct TaskDTO
  id::Union{Int, Nothing}
  user_id::Int
  title::String
  description::String
  done::Bool
end

function create_task(user_id::Int, title::String, description::String)::TaskDTO
  if UsersRepository.find(user_id) === nothing
      error("User not found")
  end

  if isempty(title)
      error("Title is required")
  end

  task = Task()
  task.user_id = SearchLight.DbId(user_id)
  task.title = title
  task.description = description
  task.done = false

  saved = TasksRepository.create(task)
  TaskDTO(Base.convert(Int, saved.id), Base.convert(Int, saved.user_id), saved.title, saved.description, saved.done)
end

function list_tasks(user_id::Int)::Vector{TaskDTO}
    if UsersRepository.find(user_id) === nothing
        error("User not found")
    end
    tasks = TasksRepository.all_for_user(user_id)
    map(t -> TaskDTO(Base.convert(Int, t.id), Base.convert(Int, t.user_id), t.title, t.description, t.done), tasks)
end

function get_task(id::Int)::Union{TaskDTO, Nothing}
  t = TasksRepository.find(id)
  if t === nothing
    return nothing
  end
  TaskDTO(Base.convert(Int, t.id), Base.convert(Int, t.user_id), t.title, t.description, t.done)
end

function update_task(id::Int, title::String, description::String)::Union{TaskDTO, Nothing}
  t = TasksRepository.find(id)
  if t === nothing
    return nothing
  end

  if isempty(title)
      error("Title cannot be empty")
  end

  t.title = title
  t.description = description
  saved = TasksRepository.update(t)
  TaskDTO(Base.convert(Int, saved.id), Base.convert(Int, saved.user_id), saved.title, saved.description, saved.done)
end

function mark_done(id::Int)::Union{TaskDTO, Nothing}
  t = TasksRepository.find(id)
  if t === nothing
    return nothing
  end

  t.done = true
  saved = TasksRepository.update(t)
  TaskDTO(Base.convert(Int, saved.id), Base.convert(Int, saved.user_id), saved.title, saved.description, saved.done)
end

function delete_task(id::Int)
  TasksRepository.delete(id)
end

end
