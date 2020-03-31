{-# LANGUAGE DeriveGeneric #-}

module RequestModels.Project.Create where

import Import

data CreateProjectPayload = CreateProjectPayload { name :: Text } deriving (Generic, Show)

instance FromJSON CreateProjectPayload where
