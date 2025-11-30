{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Handlers
    ( server
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Database.SQLite.Simple (Connection)
import Servant
import API
import Models
import Repository
import Control.Monad.Except (throwError)

server :: Connection -> Server API
server conn = usersServer conn
         :<|> tasksServer conn
         :<|> healthServer

usersServer :: Connection -> Server UserAPI
usersServer conn = createUserH conn
              :<|> getAllUsersH conn
              :<|> userOperationsH conn

userOperationsH :: Connection -> Int64 -> Server (Get '[JSON] User :<|> ReqBody '[JSON] UpdateUser :> Put '[JSON] User :<|> Delete '[JSON] NoContent :<|> "tasks" :> (ReqBody '[JSON] CreateTask :> Post '[JSON] Task :<|> Get '[JSON] [Task]))
userOperationsH conn uid = getUserH conn uid
                      :<|> updateUserH conn uid
                      :<|> deleteUserH conn uid
                      :<|> userTasksH conn uid

userTasksH :: Connection -> Int64 -> Server ("tasks" :> (ReqBody '[JSON] CreateTask :> Post '[JSON] Task :<|> Get '[JSON] [Task]))
userTasksH conn uid = createTaskH conn uid
                 :<|> getTasksByUserH conn uid

tasksServer :: Connection -> Server TaskAPI
tasksServer conn tid = getTaskH conn tid
                  :<|> updateTaskH conn tid
                  :<|> markTaskDoneH conn tid
                  :<|> deleteTaskH conn tid

healthServer :: Handler HealthStatus
healthServer = return $ HealthStatus "OK"

-- Implementation Handlers

createUserH :: Connection -> CreateUser -> Handler User
createUserH conn u = liftIO $ createUser conn u

getAllUsersH :: Connection -> Handler [User]
getAllUsersH conn = liftIO $ getAllUsers conn

getUserH :: Connection -> Int64 -> Handler User
getUserH conn uid = do
    mUser <- liftIO $ getUser conn uid
    case mUser of
        Nothing -> throwError err404
        Just u  -> return u

updateUserH :: Connection -> Int64 -> UpdateUser -> Handler User
updateUserH conn uid u = do
    mUser <- liftIO $ updateUser conn uid u
    case mUser of
        Nothing -> throwError err404
        Just user -> return user

deleteUserH :: Connection -> Int64 -> Handler NoContent
deleteUserH conn uid = do
    deleted <- liftIO $ deleteUser conn uid
    if deleted
        then return NoContent
        else throwError err404

createTaskH :: Connection -> Int64 -> CreateTask -> Handler Task
createTaskH conn uid t = do
    -- Verify user exists first
    mUser <- liftIO $ getUser conn uid
    case mUser of
        Nothing -> throwError err404
        Just _ -> liftIO $ createTask conn uid t

getTasksByUserH :: Connection -> Int64 -> Handler [Task]
getTasksByUserH conn uid = do
    -- Check user existence
    mUser <- liftIO $ getUser conn uid
    case mUser of
        Nothing -> throwError err404
        Just _ -> liftIO $ getTasksByUser conn uid

getTaskH :: Connection -> Int64 -> Handler Task
getTaskH conn tid = do
    mTask <- liftIO $ getTask conn tid
    case mTask of
        Nothing -> throwError err404
        Just t -> return t

updateTaskH :: Connection -> Int64 -> UpdateTask -> Handler Task
updateTaskH conn tid t = do
    mTask <- liftIO $ updateTask conn tid t
    case mTask of
        Nothing -> throwError err404
        Just task -> return task

markTaskDoneH :: Connection -> Int64 -> Handler Task
markTaskDoneH conn tid = do
    mTask <- liftIO $ markTaskDone conn tid
    case mTask of
        Nothing -> throwError err404
        Just task -> return task

deleteTaskH :: Connection -> Int64 -> Handler NoContent
deleteTaskH conn tid = do
    deleted <- liftIO $ deleteTask conn tid
    if deleted
        then return NoContent
        else throwError err404
