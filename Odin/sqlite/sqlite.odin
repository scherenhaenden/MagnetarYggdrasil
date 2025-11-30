package sqlite

import "core:c"

when ODIN_OS == .Windows {
	foreign import sqlite3 "sqlite3.lib"
} else when ODIN_OS == .Linux {
	foreign import sqlite3 "system:sqlite3"
} else when ODIN_OS == .Darwin {
	foreign import sqlite3 "system:sqlite3"
}

// Result Codes
OK :: 0
ROW :: 100
DONE :: 101

// Flags
OPEN_READWRITE :: 0x00000002
OPEN_CREATE    :: 0x00000004
OPEN_MEMORY    :: 0x00000080

Connection :: struct {}
Statement :: struct {}

foreign sqlite3 {
    @(link_name="sqlite3_open")
	open :: proc(filename: cstring, ppDb: ^^Connection) -> c.int ---

    @(link_name="sqlite3_open_v2")
    open_v2 :: proc(filename: cstring, ppDb: ^^Connection, flags: c.int, zVfs: cstring) -> c.int ---

    @(link_name="sqlite3_close")
	close :: proc(db: ^Connection) -> c.int ---

    @(link_name="sqlite3_prepare_v2")
	prepare_v2 :: proc(db: ^Connection, zSql: cstring, nByte: c.int, ppStmt: ^^Statement, pzTail: ^cstring) -> c.int ---

    @(link_name="sqlite3_step")
	step :: proc(stmt: ^Statement) -> c.int ---

    @(link_name="sqlite3_finalize")
	finalize :: proc(stmt: ^Statement) -> c.int ---

    @(link_name="sqlite3_exec")
	exec :: proc(db: ^Connection, sql: cstring, callback: rawptr, arg: rawptr, errmsg: ^cstring) -> c.int ---

    @(link_name="sqlite3_errmsg")
    errmsg :: proc(db: ^Connection) -> cstring ---

    @(link_name="sqlite3_changes")
    changes :: proc(db: ^Connection) -> c.int ---

    @(link_name="sqlite3_last_insert_rowid")
    last_insert_rowid :: proc(db: ^Connection) -> i64 ---

    // Bindings
    @(link_name="sqlite3_bind_int")
    bind_int :: proc(stmt: ^Statement, idx: c.int, val: c.int) -> c.int ---

    @(link_name="sqlite3_bind_int64")
    bind_int64 :: proc(stmt: ^Statement, idx: c.int, val: i64) -> c.int ---

    @(link_name="sqlite3_bind_double")
    bind_double :: proc(stmt: ^Statement, idx: c.int, val: f64) -> c.int ---

    @(link_name="sqlite3_bind_text")
    bind_text :: proc(stmt: ^Statement, idx: c.int, val: cstring, n: c.int, free_callback: rawptr) -> c.int ---

    @(link_name="sqlite3_bind_null")
    bind_null :: proc(stmt: ^Statement, idx: c.int) -> c.int ---

    // Columns
    @(link_name="sqlite3_column_int")
    column_int :: proc(stmt: ^Statement, iCol: c.int) -> c.int ---

    @(link_name="sqlite3_column_int64")
    column_int64 :: proc(stmt: ^Statement, iCol: c.int) -> i64 ---

    @(link_name="sqlite3_column_text")
    column_text :: proc(stmt: ^Statement, iCol: c.int) -> cstring ---
}

// Helpers
SQLITE_STATIC    : rawptr : rawptr(uintptr(0))
SQLITE_TRANSIENT : rawptr : rawptr(uintptr(-1))
