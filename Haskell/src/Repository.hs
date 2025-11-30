{-# LANGUAGE OverloadedStrings #-}

module Repository
    ( initializeDB
    , createUser
    , getAllUsers
    , getUser
    , updateUser
    , deleteUser
    , createTask
    , getTasksByUser
    , getTask
    , updateTask
    , markTaskDone
    , deleteTask
    ) where

import Control.Monad (void)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Models

initializeDB :: Connection -> IO ()
initializeDB conn = do
    execute_ conn "PRAGMA foreign_keys = ON;"
    execute_ conn "CREATE TABLE IF NOT EXISTS users ( \
                  \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
                  \  name TEXT NOT NULL, \
                  \  email TEXT NOT NULL UNIQUE \
                  \);"
    execute_ conn "CREATE TABLE IF NOT EXISTS tasks ( \
                  \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
                  \  user_id INTEGER NOT NULL, \
                  \  title TEXT NOT NULL, \
                  \  description TEXT NOT NULL, \
                  \  is_done BOOLEAN NOT NULL DEFAULT 0, \
                  \  FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE \
                  \);"
    execute_ conn "CREATE INDEX IF NOT EXISTS idx_tasks_user_id ON tasks(user_id);"

-- User Operations

createUser :: Connection -> CreateUser -> IO User
createUser conn (CreateUser name email) = do
    execute conn "INSERT INTO users (name, email) VALUES (?, ?)" (name, email)
    userId <- lastInsertRowId conn
    return $ User userId name email

getAllUsers :: Connection -> IO [User]
getAllUsers conn = query_ conn "SELECT id, name, email FROM users"

getUser :: Connection -> Int64 -> IO (Maybe User)
getUser conn uid = do
    users <- query conn "SELECT id, name, email FROM users WHERE id = ?" (Only uid)
    return $ listToMaybe users

updateUser :: Connection -> Int64 -> UpdateUser -> IO (Maybe User)
updateUser conn uid (UpdateUser mName mEmail) = do
    -- Simple approach: fetch, update, save. In a real app, might build dynamic query.
    mUser <- getUser conn uid
    case mUser of
        Nothing -> return Nothing
        Just (User _ currentName currentEmail) -> do
            let newName = maybe currentName id mName
            let newEmail = maybe currentEmail id mEmail
            execute conn "UPDATE users SET name = ?, email = ? WHERE id = ?" (newName, newEmail, uid)
            return $ Just (User uid newName newEmail)

deleteUser :: Connection -> Int64 -> IO Bool
deleteUser conn uid = do
    execute conn "DELETE FROM users WHERE id = ?" (Only uid)
    changes <- changes conn
    return (changes > 0)

-- Task Operations

createTask :: Connection -> Int64 -> CreateTask -> IO Task
createTask conn uid (CreateTask title desc) = do
    execute conn "INSERT INTO tasks (user_id, title, description, is_done) VALUES (?, ?, ?, ?)" (uid, title, desc, False)
    tid <- lastInsertRowId conn
    return $ Task tid uid title desc False

getTasksByUser :: Connection -> Int64 -> IO [Task]
getTasksByUser conn uid = query conn "SELECT id, user_id, title, description, is_done FROM tasks WHERE user_id = ?" (Only uid)

getTask :: Connection -> Int64 -> IO (Maybe Task)
getTask conn tid = do
    tasks <- query conn "SELECT id, user_id, title, description, is_done FROM tasks WHERE id = ?" (Only tid)
    return $ listToMaybe tasks

updateTask :: Connection -> Int64 -> UpdateTask -> IO (Maybe Task)
updateTask conn tid (UpdateTask mTitle mDesc mDone) = do
    mTask <- getTask conn tid
    case mTask of
        Nothing -> return Nothing
        Just (Task _ uid currentTitle currentDesc currentDone) -> do
            let newTitle = maybe currentTitle id mTitle
            let newDesc = maybe currentDesc id mDesc
            let newDone = maybe currentDone id mDone
            execute conn "UPDATE tasks SET title = ?, description = ?, is_done = ? WHERE id = ?" (newTitle, newDesc, newDone, tid)
            return $ Just (Task tid uid newTitle newDesc newDone)

markTaskDone :: Connection -> Int64 -> IO (Maybe Task)
markTaskDone conn tid = do
    -- We can just use updateTask logic, effectively.
    -- Or explicit SQL for efficiency if needed.
    mTask <- getTask conn tid
    case mTask of
        Nothing -> return Nothing
        Just t -> do
            execute conn "UPDATE tasks SET is_done = 1 WHERE id = ?" (Only tid)
            return $ Just (t { taskIsDone = True })

deleteTask :: Connection -> Int64 -> IO Bool
deleteTask conn tid = do
    execute conn "DELETE FROM tasks WHERE id = ?" (Only tid)
    changes <- changes conn
    return (changes > 0)
