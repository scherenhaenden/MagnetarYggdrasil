package app

import "core:encoding/json"

User :: struct {
    id: int,
    name: string,
    email: string,
}

Task :: struct {
    id: int,
    user_id: int,
    title: string,
    description: string,
    done: bool,
}

// Structs for JSON parsing
User_Payload :: struct {
    name: string,
    email: string,
}

Task_Payload :: struct {
    title: string,
    description: string,
}

Task_Update_Payload :: struct {
    title: string,
    description: string,
    done: bool,
}
