{-# LANGUAGE DeriveGeneric #-}

module RequestModels.Issue.Change where

import Import

data ChangeIssuePayload = ChangeIssuePayload {
    title :: Text
    , text :: Text
} deriving (Generic, Show)

instance FromJSON ChangeIssuePayload where
