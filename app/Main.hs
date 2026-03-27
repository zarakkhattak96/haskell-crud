{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Pool (Pool)
import Data.Time.Clock (getCurrentTime)
import Database (createConnectionPool, initDatabase)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Types.Status (notFound404)
import UserRepository
  ( createUser,
    deleteUser,
    fetchAllUsers,
    fetchUser,
    updateUser,
  )
import Web.Scotty
  ( ScottyM,
    delete,
    get,
    json,
    jsonData,
    pathParam,
    post,
    put,
    scotty,
    status,
  )

logInfo :: String -> IO ()
logInfo message = do
  now <- getCurrentTime
  putStrLn ("[INFO] " ++ show now ++ " - " ++ message)

main :: IO ()
main = do
  pool <- createConnectionPool
  initDatabase pool
  logInfo "Starting server on port 5999"
  scotty 5999 (routes pool)

routes :: Pool Connection -> ScottyM ()
routes pool = do
  get "/health" $ do
    liftIO $ logInfo "GET /health"
    json $
      object
        [ "success" .= True,
          "message" .= ("Server is health" :: String)
        ]
  get "/users" $ do
    liftIO $ logInfo "GET /users"
    users <- liftIO (fetchAllUsers pool)
    json users
  get "/users/:id" $ do
    userId <- pathParam "id"
    liftIO $ logInfo ("GET /users/" ++ show (userId :: Int))
    maybeUser <- liftIO (fetchUser pool userId)
    case maybeUser of
      Nothing -> do
        status notFound404
        json $
          object
            [ "success" .= False,
              "message" .= ("User not found" :: String)
            ]
      Just user -> json user
  post "/users" $ do
    payload <- jsonData
    liftIO $ logInfo "POST /users"
    created <- liftIO (createUser pool payload)
    json created
  put "/users/:id" $ do
    userId <- pathParam "id"
    payload <- jsonData
    liftIO $ logInfo ("PUT /users/" ++ show (userId :: Int))
    maybeUpdated <- liftIO (updateUser pool userId payload)
    case maybeUpdated of
      Nothing -> do
        status notFound404
        json $
          object
            [ "success" .= False,
              "message" .= ("User not found" :: String)
            ]
      Just user -> json user
  delete "/users/:id" $ do
    userId <- pathParam "id"
    liftIO $ logInfo ("DELETE /users/" ++ show (userId :: Int))
    deleted <- liftIO (deleteUser pool userId)
    if deleted
      then
        json $
          object
            [ "success" .= True,
              "message" .= ("User archived" :: String)
            ]
      else do
        status notFound404
        json $
          object
            [ "success" .= False,
              "message" .= ("User not found" :: String)
            ]