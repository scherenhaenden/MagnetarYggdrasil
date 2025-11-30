module TasksController

using Genie.Requests
using Genie.Renderer.Json
using ..TasksService

function create()
  try
      user_id = parse(Int, params(:id))
      payload = jsonpayload()
      title = get(payload, "title", "")
      description = get(payload, "description", "")

      task = TasksService.create_task(user_id, title, description)
      json(task, status = 201)
  catch e
      if isa(e, ErrorException) && e.msg == "User not found"
          json(Dict("error" => "User not found"), status = 404)
      else
          json(Dict("error" => string(e)), status = 400)
      end
  end
end

function index()
  try
      user_id = parse(Int, params(:id))
      tasks = TasksService.list_tasks(user_id)
      json(tasks)
  catch e
      if isa(e, ErrorException) && e.msg == "User not found"
          json(Dict("error" => "User not found"), status = 404)
      else
          json(Dict("error" => string(e)), status = 400)
      end
  end
end

function show()
  id = parse(Int, params(:id))
  task = TasksService.get_task(id)
  if task === nothing
      json(Dict("error" => "Task not found"), status = 404)
  else
      json(task)
  end
end

function update()
  try
      id = parse(Int, params(:id))
      payload = jsonpayload()
      title = get(payload, "title", "")
      description = get(payload, "description", "")

      task = TasksService.update_task(id, title, description)
      if task === nothing
          json(Dict("error" => "Task not found"), status = 404)
      else
          json(task)
      end
  catch e
      json(Dict("error" => string(e)), status = 400)
  end
end

function mark_done()
  id = parse(Int, params(:id))
  task = TasksService.mark_done(id)
  if task === nothing
      json(Dict("error" => "Task not found"), status = 404)
  else
      json(task)
  end
end

function delete()
  id = parse(Int, params(:id))
  TasksService.delete_task(id)
  Genie.Renderer.Json.json(nothing, status = 204)
end

end
