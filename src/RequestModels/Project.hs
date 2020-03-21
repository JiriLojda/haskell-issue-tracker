{-# LANGUAGE DeriveGeneric #-}

module RequestModels.Project where

import Import

data RenameProjectPayload = RenameProjectPayload { name :: Text } deriving (Generic, Show)

instance FromJSON RenameProjectPayload where