local DB = require("db.db")
local Task = require("app.models.task")

local _M = {}

function _M.create(user_id, title, description)
    local db = DB.connect()
    local res, err = db:execute("INSERT INTO tasks (user_id, title, description, done) VALUES (?, ?, ?, 0)", {user_id, title, description})
    db:close()

    if not res then return nil, err end
    return Task.new(res.last_insert_id, user_id, title, description, false), nil
end

function _M.find_by_user(user_id)
    local db = DB.connect()
    local rows, err = db:query("SELECT * FROM tasks WHERE user_id = ?", {user_id})
    db:close()

    if not rows then return {}, err end

    local tasks = {}
    for _, row in ipairs(rows) do
        table.insert(tasks, Task.new(row.id, row.user_id, row.title, row.description, row.done))
    end
    return tasks
end

function _M.find_by_id(id)
    local db = DB.connect()
    local rows, err = db:query("SELECT * FROM tasks WHERE id = ?", {id})
    db:close()

    if not rows or #rows == 0 then return nil, err end
    local row = rows[1]
    return Task.new(row.id, row.user_id, row.title, row.description, row.done)
end

function _M.update(id, title, description)
    local db = DB.connect()
    local res, err = db:execute("UPDATE tasks SET title = ?, description = ? WHERE id = ?", {title, description, id})
    db:close()

    if not res then return nil, err end
    if res.changes == 0 then return nil, "not found" end
    return true
end

function _M.mark_done(id)
    local db = DB.connect()
    local res, err = db:execute("UPDATE tasks SET done = 1 WHERE id = ?", {id})
    db:close()

    if not res then return nil, err end
    if res.changes == 0 then return nil, "not found" end
    return true
end

function _M.delete(id)
    local db = DB.connect()
    local res, err = db:execute("DELETE FROM tasks WHERE id = ?", {id})
    db:close()

    if not res then return nil, err end
    return true
end

return _M
