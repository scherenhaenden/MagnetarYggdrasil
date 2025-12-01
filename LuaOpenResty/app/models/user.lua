local _M = {}

function _M.new(id, username, email)
    return {
        id = id,
        username = username,
        email = email
    }
end

return _M
