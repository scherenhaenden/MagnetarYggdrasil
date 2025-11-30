local ffi = require("ffi")

ffi.cdef[[
    typedef struct sqlite3 sqlite3;
    typedef struct sqlite3_stmt sqlite3_stmt;

    int sqlite3_open(const char *filename, sqlite3 **ppDb);
    int sqlite3_close(sqlite3*);
    int sqlite3_prepare_v2(sqlite3 *db, const char *zSql, int nByte, sqlite3_stmt **ppStmt, const char **pzTail);
    int sqlite3_step(sqlite3_stmt*);
    int sqlite3_finalize(sqlite3_stmt *pStmt);
    int sqlite3_column_count(sqlite3_stmt *pStmt);
    int sqlite3_column_type(sqlite3_stmt *pStmt, int iCol);
    const char *sqlite3_column_name(sqlite3_stmt *pStmt, int N);
    const char *sqlite3_column_text(sqlite3_stmt *pStmt, int iCol);
    int sqlite3_column_int(sqlite3_stmt *pStmt, int iCol);
    double sqlite3_column_double(sqlite3_stmt *pStmt, int iCol);
    sqlite3_int64 sqlite3_column_int64(sqlite3_stmt *pStmt, int iCol);
    int sqlite3_bind_text(sqlite3_stmt*, int, const char*, int, void(*)(void*));
    int sqlite3_bind_int(sqlite3_stmt*, int, int);
    int sqlite3_bind_int64(sqlite3_stmt*, int, sqlite3_int64);
    int sqlite3_bind_double(sqlite3_stmt*, int, double);
    int sqlite3_bind_null(sqlite3_stmt*, int);
    int sqlite3_reset(sqlite3_stmt *pStmt);
    const char *sqlite3_errmsg(sqlite3*);
    sqlite3_int64 sqlite3_last_insert_rowid(sqlite3*);
    int sqlite3_changes(sqlite3*);
    int sqlite3_exec(sqlite3*, const char *sql, int (*callback)(void*,int,char**,char**), void *, char **errmsg);
    void sqlite3_free(void*);
]]

local clib = ffi.load("sqlite3")

local _M = {}
local mt = { __index = _M }

local SQLITE_OK = 0
local SQLITE_ROW = 100
local SQLITE_DONE = 101

function _M.open(path)
    local db_ptr = ffi.new("sqlite3*[1]")
    local code = clib.sqlite3_open(path, db_ptr)
    if code ~= SQLITE_OK then
        return nil, "Failed to open database: " .. code
    end
    return setmetatable({ db = db_ptr[0] }, mt)
end

function _M:close()
    if self.db then
        clib.sqlite3_close(self.db)
        self.db = nil
    end
end

function _M:exec(sql)
    local err_ptr = ffi.new("char*[1]")
    local code = clib.sqlite3_exec(self.db, sql, nil, nil, err_ptr)
    if code ~= SQLITE_OK then
        local err = ffi.string(err_ptr[0])
        clib.sqlite3_free(err_ptr[0])
        return false, err
    end
    return true
end

function _M:prepare(sql)
    local stmt_ptr = ffi.new("sqlite3_stmt*[1]")
    local code = clib.sqlite3_prepare_v2(self.db, sql, -1, stmt_ptr, nil)
    if code ~= SQLITE_OK then
        return nil, ffi.string(clib.sqlite3_errmsg(self.db))
    end

    local stmt = { stmt = stmt_ptr[0], db = self.db }

    function stmt:bind(params)
        if not params then return true end
        for i, v in ipairs(params) do
            local res
            if type(v) == "number" then
                if math.floor(v) == v then
                    res = clib.sqlite3_bind_int64(self.stmt, i, v)
                else
                    res = clib.sqlite3_bind_double(self.stmt, i, v)
                end
            elseif type(v) == "string" then
                res = clib.sqlite3_bind_text(self.stmt, i, v, #v, nil) -- Transients are okay if we execute immediately? No, safer to use -1 (static) if string lives long enough, or copy. But nil destructor usually implies transient/static depending on impl. Actually default is static. -1 is SQLITE_STATIC. 0 is pointer to destructor.
                -- To be safe with Lua strings that might be GC'd, let's assume transient (SQLITE_TRANSIENT is -1 cast to ptr).
                -- But pure FFI, we can just pass the string. LuaJIT creates a cdata.
                -- Let's use internal copy strategy if needed.
                -- For now, passing -1 (SQLITE_STATIC) is risky if string is GC'd.
                -- Using SQLITE_TRANSIENT constant: typedef void (*sqlite3_destructor_type)(void*); #define SQLITE_TRANSIENT ((sqlite3_destructor_type)-1)
                res = clib.sqlite3_bind_text(self.stmt, i, v, #v, ffi.cast("void(*)(void*)", -1))
            elseif type(v) == "boolean" then
                res = clib.sqlite3_bind_int(self.stmt, i, v and 1 or 0)
            elseif v == nil then
                res = clib.sqlite3_bind_null(self.stmt, i)
            end
            if res ~= SQLITE_OK then return false, "Bind error" end
        end
        return true
    end

    function stmt:step()
        return clib.sqlite3_step(self.stmt)
    end

    function stmt:finalize()
        clib.sqlite3_finalize(self.stmt)
    end

    function stmt:columns()
        local count = clib.sqlite3_column_count(self.stmt)
        local cols = {}
        for i = 0, count - 1 do
            table.insert(cols, ffi.string(clib.sqlite3_column_name(self.stmt, i)))
        end
        return cols
    end

    function stmt:get_row()
        local count = clib.sqlite3_column_count(self.stmt)
        local row = {}
        local col_names = self:columns()
        for i = 0, count - 1 do
            local type = clib.sqlite3_column_type(self.stmt, i)
            local name = col_names[i+1]
            local val
            if type == 1 then -- INTEGER
                val = tonumber(clib.sqlite3_column_int64(self.stmt, i))
            elseif type == 2 then -- FLOAT
                val = tonumber(clib.sqlite3_column_double(self.stmt, i))
            elseif type == 3 then -- TEXT
                val = ffi.string(clib.sqlite3_column_text(self.stmt, i))
            elseif type == 4 then -- BLOB
                val = ffi.string(clib.sqlite3_column_text(self.stmt, i)) -- Treating blob as text for now
            elseif type == 5 then -- NULL
                val = nil -- or box.NULL
            end
            row[name] = val
        end
        return row
    end

    return stmt
end

function _M:query(sql, params)
    local stmt, err = self:prepare(sql)
    if not stmt then return nil, err end

    local ok, bind_err = stmt:bind(params)
    if not ok then
        stmt:finalize()
        return nil, bind_err
    end

    local rows = {}
    while true do
        local code = stmt:step()
        if code == SQLITE_ROW then
            table.insert(rows, stmt:get_row())
        elseif code == SQLITE_DONE then
            break
        else
            stmt:finalize()
            return nil, "Step error: " .. code
        end
    end
    stmt:finalize()
    return rows
end

function _M:execute(sql, params)
    local stmt, err = self:prepare(sql)
    if not stmt then return nil, err end

    local ok, bind_err = stmt:bind(params)
    if not ok then
        stmt:finalize()
        return nil, bind_err
    end

    local code = stmt:step()
    stmt:finalize()

    if code ~= SQLITE_DONE and code ~= SQLITE_ROW then
        return nil, "Execute error: " .. code
    end

    return {
        last_insert_id = tonumber(clib.sqlite3_last_insert_rowid(self.db)),
        changes = tonumber(clib.sqlite3_changes(self.db))
    }
end

return _M
