module Repositories.IssueComment where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe

import Import hiding ((.))
import SharedTypes


getIssueComment :: IssueCommentId -> DBAction (Entity IssueComment)
getIssueComment = (maybeToExceptT NoIssueComment) . MaybeT . getEntity

replaceIssueComment :: IssueCommentId -> IssueComment -> DBAction (Entity IssueComment)
replaceIssueComment commentId comment = do
    lift $ replace commentId comment
    return $ Entity commentId comment
