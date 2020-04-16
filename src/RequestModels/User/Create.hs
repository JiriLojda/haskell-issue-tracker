{-# LANGUAGE DeriveGeneric #-}

module RequestModels.User.Create where

import Import

data CreateUserPayload = CreateUserPayload {
    firstName :: Text
    , lastName :: Text
} deriving (Generic)

instance FromJSON CreateUserPayload where

toUser :: CreateUserPayload -> User
toUser CreateUserPayload{ firstName = firstName, lastName = lastName} = User firstName lastName
