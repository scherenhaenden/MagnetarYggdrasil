{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module API
    ( API
    , UserAPI
    , TaskAPI
    , HealthAPI
    ) where

import Data.Int (Int64)
import Models (User, CreateUser, UpdateUser, Task, CreateTask, UpdateTask)
import Servant
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

type API = "users" :> UserAPI
      :<|> "tasks" :> TaskAPI
      :<|> "health" :> HealthAPI

-- Users API
-- POST /users
-- GET /users
-- GET /users/{id}
-- PUT /users/{id}
-- DELETE /users/{id}
-- POST /users/{id}/tasks
-- GET /users/{id}/tasks

-- Tasks
-- POST /users/{id}/tasks
-- GET /users/{id}/tasks
-- GET /tasks/{tid}
-- PUT /tasks/{tid}
-- PATCH /tasks/{tid}/done
-- DELETE /tasks/{tid}

type UserAPI =
    ReqBody '[JSON] CreateUser :> Post '[JSON] User
    :<|> Get '[JSON] [User]
    :<|> Capture "id" Int64 :>
        (    Get '[JSON] User
        :<|> ReqBody '[JSON] UpdateUser :> Put '[JSON] User
        :<|> Delete '[JSON] NoContent
        :<|> "tasks" :>
             (    ReqBody '[JSON] CreateTask :> Post '[JSON] Task
             :<|> Get '[JSON] [Task]
             )
        )

type TaskAPI =
    Capture "tid" Int64 :>
        (    Get '[JSON] Task
        :<|> ReqBody '[JSON] UpdateTask :> Put '[JSON] Task
        :<|> "done" :> Patch '[JSON] Task
        :<|> Delete '[JSON] NoContent
        )

type HealthAPI = Get '[JSON] HealthStatus

data HealthStatus = HealthStatus { status :: String }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
