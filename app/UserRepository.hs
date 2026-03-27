{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module UserRepository
  ( User (..),
    CreateUserRequest (..),
    UpdateUserRequest (..),
    fetchAllUsers,
    fetchUser,
    createUser,
    updateUser,
    deleteUser,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Pool (Pool, withResource)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import GHC.Generics (Generic)

data User = User
  { id :: Int,
    fullName :: String,
    designation :: String,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    isArchived :: Bool
  }
  deriving (Show, Generic)

instance ToJSON User

instance FromRow User where
  fromRow =
    User
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

data CreateUserRequest = CreateUserRequest
  { createFullName :: String,
    createDesignation :: String
  }
  deriving (Show)

instance FromJSON CreateUserRequest where
  parseJSON = Aeson.withObject "CreateUserRequest" $ \obj ->
    CreateUserRequest
      <$> obj Aeson..: "fullName"
      <*> obj Aeson..: "designation"

data UpdateUserRequest = UpdateUserRequest
  { updateFullName :: String,
    updateDesignation :: String,
    updateIsArchived :: Bool
  }
  deriving (Show)

instance FromJSON UpdateUserRequest where
  parseJSON = Aeson.withObject "UpdateUserRequest" $ \obj ->
    UpdateUserRequest
      <$> obj Aeson..: "fullName"
      <*> obj Aeson..: "designation"
      <*> obj Aeson..: "isArchived"

fetchAllUsers :: Pool Connection -> IO [User]
fetchAllUsers pool =
  withResource pool $ \conn ->
    query_
      conn
      "SELECT id, \"fullName\", designation, \"createdAt\", \"updatedAt\", \"isArchived\" \
      \FROM users WHERE \"isArchived\" = FALSE ORDER BY id ASC"

fetchUser :: Pool Connection -> Int -> IO (Maybe User)
fetchUser pool userId =
  withResource pool $ \conn -> do
    rows <-
      query
        conn
        "SELECT id, \"fullName\", designation, \"createdAt\", \"updatedAt\", \"isArchived\" \
        \FROM users WHERE id = ? AND \"isArchived\" = FALSE LIMIT 1"
        (Only userId)
    pure $
      case rows of
        [] -> Nothing
        user : _ -> Just user

createUser :: Pool Connection -> CreateUserRequest -> IO User
createUser pool payload =
  withResource pool $ \conn -> do
    rows <-
      query
        conn
        "INSERT INTO users (\"fullName\", designation, \"createdAt\", \"updatedAt\", \"isArchived\") \
        \VALUES (?, ?, NOW(), NOW(), FALSE) \
        \RETURNING id, \"fullName\", designation, \"createdAt\", \"updatedAt\", \"isArchived\""
        (createFullName payload, createDesignation payload)
    case rows of
      [] -> fail "User was not created."
      user : _ -> pure user

updateUser :: Pool Connection -> Int -> UpdateUserRequest -> IO (Maybe User)
updateUser pool userId payload =
  withResource pool $ \conn -> do
    rows <-
      query
        conn
        "UPDATE users \
        \SET \"fullName\" = ?, designation = ?, \"isArchived\" = ?, \"updatedAt\" = NOW() \
        \WHERE id = ? AND \"isArchived\" = FALSE \
        \RETURNING id, \"fullName\", designation, \"createdAt\", \"updatedAt\", \"isArchived\""
        (updateFullName payload, updateDesignation payload, updateIsArchived payload, userId)
    pure $
      case rows of
        [] -> Nothing
        user : _ -> Just user

deleteUser :: Pool Connection -> Int -> IO Bool
deleteUser pool userId =
  withResource pool $ \conn -> do
    affectedRows <-
      execute
        conn
        "UPDATE users SET \"isArchived\" = TRUE, \"updatedAt\" = NOW() WHERE id = ? AND \"isArchived\" = FALSE"
        (Only userId)
    pure (affectedRows > 0)
