module Repositories.Issue where

import Control.Monad.Trans.Maybe

import Import hiding ((.))
import SharedTypes

getIssue :: IssueId -> DBAction (Entity Issue)
getIssue = (maybeToExceptT NoIssue) . MaybeT . getEntity

updateIssue :: IssueId -> [Update Issue] -> DBAction (Entity Issue)
updateIssue issueId mods = do
    entity <- lift $ updateGet issueId mods
    return $ Entity issueId entity
