module main

import vweb
import os
import api
import service
import repository

fn main() {
	db_path := 'magnetar.db'

	// Ensure DB is created
	if !os.exists(db_path) {
		println('Creating database at $db_path')
		os.create(db_path) or { panic(err) }
	}

	repo := repository.new_repository(db_path) or {
		panic(err)
	}

	svc := service.new_service(repo)
	mut app := api.new_app(svc)

	println('Starting server on http://localhost:8080')
	vweb.run(&app, 8080)
}
