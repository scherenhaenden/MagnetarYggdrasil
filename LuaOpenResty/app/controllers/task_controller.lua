local cjson = require("cjson")
local service = require("app.services.task_service")

local _M = {}

local function read_body()
    ngx.req.read_body()
    local body = ngx.req.get_body_data()
    if not body then return {} end
    return cjson.decode(body)
end

function _M.create_for_user(user_id)
    local body = read_body()
    local task, err = service.create_task(tonumber(user_id), body)

    if not task then
        if err == "User not found" then
            ngx.status = 404
            ngx.say(cjson.encode({ error = err }))
            return ngx.exit(404)
        end
        ngx.status = 400
        ngx.say(cjson.encode({ error = err }))
        return ngx.exit(400)
    end

    ngx.status = 201
    ngx.say(cjson.encode(task))
    return ngx.exit(201)
end

function _M.list_for_user(user_id)
    local tasks, err = service.get_user_tasks(tonumber(user_id))
    if err == "User not found" then
         ngx.status = 404
         ngx.say(cjson.encode({ error = err }))
         return ngx.exit(404)
    end

    if err then
        ngx.status = 500
        ngx.say(cjson.encode({ error = err }))
        return ngx.exit(500)
    end

    ngx.status = 200
    ngx.say(cjson.encode(tasks))
    return ngx.exit(200)
end

function _M.get(id)
    local task, err = service.get_task(tonumber(id))
    if not task then
        ngx.status = 404
        ngx.say(cjson.encode({ error = "Task not found" }))
        return ngx.exit(404)
    end
    ngx.status = 200
    ngx.say(cjson.encode(task))
    return ngx.exit(200)
end

function _M.update(id)
    local body = read_body()
    local task, err = service.update_task(tonumber(id), body)

    if not task then
        if err == "not found" then
            ngx.status = 404
            return ngx.exit(404)
        end
        ngx.status = 400
        ngx.say(cjson.encode({ error = err }))
        return ngx.exit(400)
    end

    ngx.status = 200
    ngx.say(cjson.encode(task))
    return ngx.exit(200)
end

function _M.mark_done(id)
    local task, err = service.mark_done(tonumber(id))
    if not task then
        if err == "not found" then
            ngx.status = 404
            return ngx.exit(404)
        end
        ngx.status = 500
        ngx.say(cjson.encode({ error = err }))
        return ngx.exit(500)
    end

    ngx.status = 200
    ngx.say(cjson.encode(task))
    return ngx.exit(200)
end

function _M.delete(id)
    local ok, err = service.delete_task(tonumber(id))
    if not ok then
        ngx.status = 500
        ngx.say(cjson.encode({ error = err }))
        return ngx.exit(500)
    end
    ngx.status = 204
    return ngx.exit(204)
end

return _M
