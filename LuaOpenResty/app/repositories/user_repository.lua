local DB = require("db.db")
local User = require("app.models.user")

local _M = {}

function _M.create(username, email)
    local db = DB.connect()
    local res, err = db:execute("INSERT INTO users (username, email) VALUES (?, ?)", {username, email})
    db:close()

    if not res then return nil, err end
    return User.new(res.last_insert_id, username, email), nil
end

function _M.find_all()
    local db = DB.connect()
    local rows, err = db:query("SELECT id, username, email FROM users")
    db:close()

    if not rows then return {}, err end

    local users = {}
    for _, row in ipairs(rows) do
        table.insert(users, User.new(row.id, row.username, row.email))
    end
    return users
end

function _M.find_by_id(id)
    local db = DB.connect()
    local rows, err = db:query("SELECT id, username, email FROM users WHERE id = ?", {id})
    db:close()

    if not rows or #rows == 0 then return nil, err end
    local row = rows[1]
    return User.new(row.id, row.username, row.email)
end

function _M.update(id, username)
    local db = DB.connect()
    local res, err = db:execute("UPDATE users SET username = ? WHERE id = ?", {username, id})
    db:close()

    if not res then return nil, err end
    if res.changes == 0 then return nil, "not found" end
    return true
end

function _M.delete(id)
    local db = DB.connect()
    local res, err = db:execute("DELETE FROM users WHERE id = ?", {id})
    db:close()

    if not res then return nil, err end
    return true
end

return _M
