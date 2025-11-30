local cjson = require("cjson")

local _M = {}

function _M.health()
    ngx.status = 200
    ngx.header.content_type = "application/json"
    ngx.say(cjson.encode({ status = "ok", version = "1.0.0" }))
    return ngx.exit(200)
end

return _M
