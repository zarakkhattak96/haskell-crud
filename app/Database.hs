{-# LANGUAGE OverloadedStrings #-}

module Database
  ( createConnectionPool,
    initDatabase,
  )
where

import Configuration.Dotenv (defaultConfig, loadFile)
import qualified Data.ByteString.Char8 as BS8
import Data.Pool (Pool, defaultPoolConfig, newPool, setNumStripes, withResource)
import Database.PostgreSQL.Simple
import System.Environment (lookupEnv)

requireEnv :: String -> IO String
requireEnv key = do
  maybeValue <- lookupEnv key
  case maybeValue of
    Just value -> pure value
    Nothing -> fail ("Missing required environment variable: " ++ key)

createConnectionPool :: IO (Pool Connection)
createConnectionPool = do
  _ <- loadFile defaultConfig
  dbHost <- requireEnv "DB_HOST"
  dbPort <- requireEnv "DB_PORT"
  dbUser <- requireEnv "DB_USER"
  dbPassword <- requireEnv "DB_PASSWORD"
  dbName <- requireEnv "DB_NAME"
  let connectionString =
        "host="
          ++ dbHost
          ++ " port="
          ++ dbPort
          ++ " user="
          ++ dbUser
          ++ " password="
          ++ dbPassword
          ++ " dbname="
          ++ dbName
  newPool $
    setNumStripes
      (Just 1)
      ( defaultPoolConfig
          (connectPostgreSQL (BS8.pack connectionString))
          close
          10
          10
      )

initDatabase :: Pool Connection -> IO ()
initDatabase pool =
  withResource pool $ \conn ->
    do
      _ <-
        execute_
          conn
          ( mconcat
              [ "CREATE TABLE IF NOT EXISTS users (",
                "id SERIAL PRIMARY KEY, ",
                "\"fullName\" TEXT NOT NULL, ",
                "designation TEXT NOT NULL, ",
                "\"createdAt\" TIMESTAMPTZ NOT NULL DEFAULT NOW(), ",
                "\"updatedAt\" TIMESTAMPTZ NOT NULL DEFAULT NOW(), ",
                "\"isArchived\" BOOLEAN NOT NULL DEFAULT FALSE",
                ")"
              ]
          )
      pure ()
