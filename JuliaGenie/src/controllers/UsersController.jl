module UsersController

using Genie.Requests
using Genie.Renderer.Json
using ..UsersService

function create()
  try
      payload = jsonpayload()
      username = get(payload, "username", "")
      email = get(payload, "email", "")

      user = UsersService.create_user(username, email)
      json(user, status = 201)
  catch e
      json(Dict("error" => string(e)), status = 400)
  end
end

function index()
  users = UsersService.list_users()
  json(users)
end

function show()
  id = parse(Int, params(:id))
  user = UsersService.get_user(id)
  if user === nothing
      json(Dict("error" => "User not found"), status = 404)
  else
      json(user)
  end
end

function update()
  try
      id = parse(Int, params(:id))
      payload = jsonpayload()
      username = get(payload, "username", "")

      user = UsersService.update_user(id, username)
      if user === nothing
          json(Dict("error" => "User not found"), status = 404)
      else
          json(user)
      end
  catch e
      json(Dict("error" => string(e)), status = 400)
  end
end

function delete()
  id = parse(Int, params(:id))
  UsersService.delete_user(id)
  Genie.Renderer.Json.json(nothing, status = 204)
end

end
