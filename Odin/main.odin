package main

import "core:fmt"
import "server"
import "app"

main :: proc() {
    fmt.println("Starting MagnetarYggdrasil Odin Server...")

    app.init_db() // Default to "magnetar.db"

    // Start server on port 8080
    server.listen_and_serve(8080, app.handle_request)
}
