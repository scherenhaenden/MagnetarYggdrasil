local cjson = require("cjson")
local service = require("app.services.user_service")

local _M = {}

local function read_body()
    ngx.req.read_body()
    local body = ngx.req.get_body_data()
    if not body then return {} end
    return cjson.decode(body)
end

function _M.create()
    local body = read_body()
    local user, err = service.create_user(body)

    if not user then
        ngx.status = 400
        ngx.say(cjson.encode({ error = err }))
        return ngx.exit(400)
    end

    ngx.status = 201
    ngx.say(cjson.encode(user))
    return ngx.exit(201)
end

function _M.list()
    local users, err = service.get_all_users()
    if err then
        ngx.status = 500
        ngx.say(cjson.encode({ error = err }))
        return ngx.exit(500)
    end
    ngx.status = 200
    ngx.say(cjson.encode(users))
    return ngx.exit(200)
end

function _M.get(id)
    local user, err = service.get_user(tonumber(id))
    if not user then
        ngx.status = 404
        ngx.say(cjson.encode({ error = "User not found" }))
        return ngx.exit(404)
    end
    ngx.status = 200
    ngx.say(cjson.encode(user))
    return ngx.exit(200)
end

function _M.update(id)
    local body = read_body()
    local user, err = service.update_user(tonumber(id), body)

    if not user then
        if err == "not found" then
            ngx.status = 404
            return ngx.exit(404)
        end
        ngx.status = 400
        ngx.say(cjson.encode({ error = err }))
        return ngx.exit(400)
    end

    ngx.status = 200
    ngx.say(cjson.encode(user))
    return ngx.exit(200)
end

function _M.delete(id)
    local ok, err = service.delete_user(tonumber(id))
    if not ok then
        ngx.status = 500
        ngx.say(cjson.encode({ error = err }))
        return ngx.exit(500)
    end
    ngx.status = 204
    return ngx.exit(204)
end

return _M
