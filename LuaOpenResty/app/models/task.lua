local _M = {}

function _M.new(id, user_id, title, description, done)
    return {
        id = id,
        user_id = user_id,
        title = title,
        description = description,
        done = done == 1 or done == true -- normalized
    }
end

return _M
