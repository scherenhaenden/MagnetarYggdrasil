import std/json

type
  Task* = object
    id*: int
    user_id*: int
    title*: string
    description*: string
    done*: bool

  TaskCreate* = object
    title*: string
    description*: string

  TaskUpdate* = object
    title*: Option[string]
    description*: Option[string]
    done*: Option[bool]

import std/options
