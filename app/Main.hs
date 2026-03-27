{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Time.Clock (getCurrentTime)
import Web.Scotty (get, json, scotty)

logInfo :: String -> IO ()
logInfo message = do
  now <- getCurrentTime
  putStrLn ("[INFO] " ++ show now ++ " - " ++ message)

main :: IO ()
main = do
  logInfo "Starting server on port 5999"
  scotty 5999 $ do
    get "/health" $ do
      liftIO $ logInfo "GET /health"
      json $
        object
          [ "success" .= True,
            "message" .= ("Server is health" :: String)
          ]