{-# LANGUAGE DeriveGeneric #-}

module RequestModels.Issue.Create where

import Import

data CreateIssuePayload = CreateIssuePayload { 
    title :: Text
    , text :: Text
    , step :: WorkflowStepId
} deriving (Generic, Show)

instance FromJSON CreateIssuePayload where
