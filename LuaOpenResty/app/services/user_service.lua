local repo = require("app.repositories.user_repository")

local _M = {}

function _M.create_user(params)
    if not params.username or not params.email then
        return nil, "Missing username or email"
    end
    return repo.create(params.username, params.email)
end

function _M.get_all_users()
    return repo.find_all()
end

function _M.get_user(id)
    return repo.find_by_id(id)
end

function _M.update_user(id, params)
    if not params.username then
        return nil, "Missing username"
    end
    local user = repo.find_by_id(id)
    if not user then return nil, "not found" end

    local ok, err = repo.update(id, params.username)
    if not ok then return nil, err end

    user.username = params.username
    return user
end

function _M.delete_user(id)
    return repo.delete(id)
end

return _M
