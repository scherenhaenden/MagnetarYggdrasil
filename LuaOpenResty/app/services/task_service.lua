local repo = require("app.repositories.task_repository")
local user_repo = require("app.repositories.user_repository")

local _M = {}

function _M.create_task(user_id, params)
    if not params.title or not params.description then
        return nil, "Missing title or description"
    end

    -- Check user exists
    local user = user_repo.find_by_id(user_id)
    if not user then return nil, "User not found" end

    return repo.create(user_id, params.title, params.description)
end

function _M.get_user_tasks(user_id)
    local user = user_repo.find_by_id(user_id)
    if not user then return nil, "User not found" end
    return repo.find_by_user(user_id)
end

function _M.get_task(id)
    return repo.find_by_id(id)
end

function _M.update_task(id, params)
    if not params.title or not params.description then
        return nil, "Missing title or description"
    end

    local task = repo.find_by_id(id)
    if not task then return nil, "not found" end

    local ok, err = repo.update(id, params.title, params.description)
    if not ok then return nil, err end

    task.title = params.title
    task.description = params.description
    return task
end

function _M.mark_done(id)
    local task = repo.find_by_id(id)
    if not task then return nil, "not found" end

    local ok, err = repo.mark_done(id)
    if not ok then return nil, err end

    task.done = true
    return task
end

function _M.delete_task(id)
    return repo.delete(id)
end

return _M
