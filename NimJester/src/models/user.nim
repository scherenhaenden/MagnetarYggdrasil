import std/json

type
  User* = object
    id*: int
    email*: string
    name*: string

  UserCreate* = object
    email*: string
    name*: string

  UserUpdate* = object
    email*: Option[string]
    name*: Option[string]

import std/options
