module Services.Utils where

import Control.Monad.Trans.Maybe

import SharedTypes
import Import

runProjectDB :: ProjectId -> (E Project -> DBAction a) -> Handler (Maybe a)
runProjectDB pId action = runDB $ runMaybeT $ do
    project <- findNotArchivedProject pId
    action project

runIssueDB :: ProjectId -> IssueId -> (E Project -> E Issue -> DBAction a) -> Handler (Maybe a)
runIssueDB pId issueId action = runProjectDB pId $ \project -> do
    issue <- MaybeT $ getEntity issueId
    action project issue

runIssueCommentDB :: ProjectId -> IssueId -> IssueCommentId -> (E Project -> E Issue -> E IssueComment -> DBAction a) -> Handler (Maybe a)
runIssueCommentDB pId issueId commentId action = runIssueDB pId issueId $ \project issue -> do
    comment <- MaybeT $ getEntity commentId
    action project issue comment


findNotArchivedProject :: ProjectId -> DBAction (Entity Project)
findNotArchivedProject pId = do
    project <- MaybeT $ getEntity $ pId
    case projectIsArchived $ entityVal project of
        True -> mzero
        False -> return project

toNoProject :: Maybe a -> Either ErrorReason a
toNoProject Nothing = Left NoProject
toNoProject (Just x) = Right x

type E = Entity