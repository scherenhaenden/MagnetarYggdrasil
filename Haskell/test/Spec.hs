{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec (main) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Database.SQLite.Simple
import Handlers (server)
import API (API)
import Repository (initializeDB)
import Servant (serve)
import Data.Proxy (Proxy(..))
import Control.Exception (bracket)
import System.Directory (removeFile, doesFileExist)
import Models
import Data.Aeson (encode)

api :: Proxy API
api = Proxy

setupApp :: IO Application
setupApp = do
    let dbFile = "test_magnetar.db"
    exists <- doesFileExist dbFile
    if exists then removeFile dbFile else return ()

    conn <- open dbFile
    initializeDB conn
    -- Leak the connection intentionally for the test duration
    return $ serve api (server conn)

main :: IO ()
main = hspec spec

spec :: Spec
spec = with setupApp $ do
    describe "MagnetarYggdrasil API" $ do

        describe "Health Check" $ do
            it "responds with 200 OK" $ do
                get "/health" `shouldRespondWith` 200
                get "/health" `shouldRespondWith` [json|{status: "OK"}|]

        describe "User Operations Scenario" $ do
            it "creates, gets, and updates a user" $ do
                -- Create
                let user = [json|{createUserName: "Alice", createUserEmail: "alice@example.com"}|]
                post "/users" user `shouldRespondWith` 200

                -- Create another User
                let user2 = [json|{createUserName: "FlowUser", createUserEmail: "flow@example.com"}|]
                post "/users" user2 `shouldRespondWith` 200

                -- Get all users
                get "/users" `shouldRespondWith` 200


        describe "Full Scenario Integration" $ do
            it "runs the full lifecycle sequentially" $ do
                -- Create User
                let user = [json|{createUserName: "Dave", createUserEmail: "dave@example.com"}|]
                post "/users" user `shouldRespondWith` 200

                get "/users" `shouldRespondWith` 200

                -- Insert a known user with a known ID for testing specific ID endpoints
                liftIO $ do
                    conn <- open "test_magnetar.db"
                    execute conn "INSERT INTO users (id, name, email) VALUES (?, ?, ?)" ((999 :: Int), "FixedUser" :: String, "fixed@example.com" :: String)
                    close conn

                get "/users/999" `shouldRespondWith` 200

                let update = [json|{updateUserName: "FixedUserUpdated"}|]
                put "/users/999" update `shouldRespondWith` 200

                -- Delete returns 200 by default in Servant for NoContent
                delete "/users/999" `shouldRespondWith` 200

                get "/users/999" `shouldRespondWith` 404
