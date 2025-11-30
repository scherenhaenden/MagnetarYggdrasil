{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models
    ( User(..)
    , CreateUser(..)
    , UpdateUser(..)
    , Task(..)
    , CreateTask(..)
    , UpdateTask(..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Database.SQLite.Simple.FromRow (FromRow(..), field)
import Database.SQLite.Simple.ToRow (ToRow(..), toRow)
import GHC.Generics (Generic)

-- User

data User = User
    { userId    :: Int64
    , userName  :: Text
    , userEmail :: Text
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field

instance ToRow User where
    toRow (User id_ name email) = toRow (id_, name, email)

data CreateUser = CreateUser
    { createUserName  :: Text
    , createUserEmail :: Text
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

data UpdateUser = UpdateUser
    { updateUserName  :: Maybe Text
    , updateUserEmail :: Maybe Text
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- Task

data Task = Task
    { taskId          :: Int64
    , taskUserId      :: Int64
    , taskTitle       :: Text
    , taskDescription :: Text
    , taskIsDone      :: Bool
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance FromRow Task where
    fromRow = Task <$> field <*> field <*> field <*> field <*> field

instance ToRow Task where
    toRow (Task id_ uid title desc done) = toRow (id_, uid, title, desc, done)

data CreateTask = CreateTask
    { createTaskTitle       :: Text
    , createTaskDescription :: Text
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

data UpdateTask = UpdateTask
    { updateTaskTitle       :: Maybe Text
    , updateTaskDescription :: Maybe Text
    , updateTaskIsDone      :: Maybe Bool
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
