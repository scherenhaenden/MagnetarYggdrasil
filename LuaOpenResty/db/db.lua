local sqlite = require("lib.sqlite")
local config = {
    path = "magnetar.db"
}

local DB = {}

function DB.connect()
    local db, err = sqlite.open(config.path)
    if not db then
        error("Could not connect to database: " .. err)
    end
    -- Enable foreign keys
    db:exec("PRAGMA foreign_keys = ON;")
    return db
end

return DB
