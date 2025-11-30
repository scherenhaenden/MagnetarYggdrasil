{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.SQLite.Simple (open, close)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import API (API)
import Handlers (server)
import Repository (initializeDB)
import Data.Proxy (Proxy(..))
import Control.Exception (bracket)

api :: Proxy API
api = Proxy

app :: Database.SQLite.Simple.Connection -> Application
app conn = serve api (server conn)

main :: IO ()
main = do
    putStrLn "Starting server on port 8080..."
    bracket (open "magnetar.db") close $ \conn -> do
        initializeDB conn
        run 8080 (logStdoutDev $ app conn)
