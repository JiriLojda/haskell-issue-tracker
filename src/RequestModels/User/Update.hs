{-# LANGUAGE DeriveGeneric #-}

module RequestModels.User.Update where

import Import

data UpdateUserPayload = UpdateUserPayload {
    firstName :: Text
    , lastName :: Text
} deriving (Generic)

instance FromJSON UpdateUserPayload where

toUser :: UpdateUserPayload -> User
toUser UpdateUserPayload{ firstName = firstName, lastName = lastName} = User firstName lastName
