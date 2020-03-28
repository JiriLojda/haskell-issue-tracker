module Services.Utils where

import Control.Monad.Trans.Except

import SharedTypes
import Import
import Repositories.Project
import Repositories.Issue
import Repositories.IssueComment

runAllProjectDB :: ProjectId -> (E Project -> DBAction a) -> ServiceReturn a
runAllProjectDB pId action = runDB $ runExceptT $ do
    project <- getProject pId
    action project

runProjectDB :: ProjectId -> (E Project -> DBAction a) -> ServiceReturn a
runProjectDB pId action = runDB $ runExceptT $ do
    project <- findNotArchivedProject pId
    action project

runIssueDB :: ProjectId -> IssueId -> (E Project -> E Issue -> DBAction a) -> ServiceReturn a
runIssueDB pId issueId action = runProjectDB pId $ \project -> do
    issue <- getIssue issueId
    action project issue

runIssueCommentDB :: ProjectId -> IssueId -> IssueCommentId -> (E Project -> E Issue -> E IssueComment -> DBAction a) -> ServiceReturn a
runIssueCommentDB pId issueId commentId action = runIssueDB pId issueId $ \project issue -> do
    comment <- getIssueComment commentId
    action project issue comment

type E = Entity