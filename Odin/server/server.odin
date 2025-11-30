package server

import "core:net"
import "core:fmt"
import "core:strings"
import "core:io"
import "core:os"

Request :: struct {
    method: string,
    path: string,
    body: string,
}

Response :: struct {
    status: int,
    body: string,
}

Handle_Func :: proc(req: Request) -> Response

listen_and_serve :: proc(port: int, handler: Handle_Func) {
    endpoint := net.Endpoint{
        address = net.IP4_Any,
        port = port,
    }

    listen_socket, err := net.listen_tcp(endpoint)
    if err != nil {
        fmt.println("Error listening:", err)
        return
    }
    defer net.close(listen_socket)

    fmt.printf("Server listening on port %d\n", port)

    for {
        client_socket, client_endpoint, accept_err := net.accept_tcp(listen_socket)
        if accept_err != nil {
            fmt.println("Error accepting:", accept_err)
            continue
        }

        handle_connection(client_socket, handler)
    }
}

handle_connection :: proc(sock: net.TCP_Socket, handler: Handle_Func) {
    defer net.close(sock)

    buf: [4096]byte
    n, err := net.recv_tcp(sock, buf[:])
    if err != nil {
        return
    }

    request_data := string(buf[:n])

    // Parse Request
    req := parse_request(request_data)

    // Call Handler
    res := handler(req)

    // Send Response
    send_response(sock, res)
}

parse_request :: proc(data: string) -> Request {
    req: Request

    lines := strings.split(data, "\r\n")
    defer delete(lines)

    if len(lines) > 0 {
        first_line := strings.split(lines[0], " ")
        defer delete(first_line)
        if len(first_line) >= 2 {
            req.method = first_line[0]
            req.path = first_line[1]
        }
    }

    // Find body
    parts := strings.split(data, "\r\n\r\n")
    defer delete(parts)
    if len(parts) > 1 {
        req.body = parts[1]
    }

    return req
}

send_response :: proc(sock: net.TCP_Socket, res: Response) {
    status_text: string
    switch res.status {
        case 200: status_text = "OK"
        case 201: status_text = "Created"
        case 204: status_text = "No Content"
        case 400: status_text = "Bad Request"
        case 404: status_text = "Not Found"
        case 500: status_text = "Internal Server Error"
        case: status_text = "Unknown"
    }

    header := fmt.tprintf("HTTP/1.1 %d %s\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n", res.status, status_text, len(res.body))

    net.send_tcp(sock, transmute([]byte)header)
    net.send_tcp(sock, transmute([]byte)res.body)
}
