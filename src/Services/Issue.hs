module Services.Issue
(
    createIssue
    , getAllIssues
    , changeIssue
    , deleteIssue
    , getIssue
    , ErrorReason
) where

import Data.UUID.V4
import Data.UUID
import qualified Data.List as L
import Data.Map (toList)
import Control.Monad.Trans.Maybe

import Import hiding (map, (.), toList)
import Services.Project
import RequestModels.Issue

data ErrorReason = NoProject | NoIssue

createIssue :: ProjectId -> Issue -> Handler (Either ErrorReason (Entity Issue))
createIssue pId issue = do
    newId <- liftIO nextRandom
    let issueId = IssueKey $ toText $ newId
    result <- runProjectIssuesDB pId $ \project -> do
        lift $ insertKey issueId issue
        let existingIssueIds = projectIssueIds $ entityVal project
        lift $ update pId [ ProjectIssueIds =. (issueId : existingIssueIds) ]
        return $ Entity issueId issue
    return $ toNoProject result

getAllIssues :: ProjectId -> Handler (Either ErrorReason ([Entity Issue]))
getAllIssues pId = do
    issues <- runProjectIssuesDB pId $ \project -> do
        result <- lift $ getMany $ projectIssueIds $ entityVal project
        return . (map (uncurry Entity)) . toList $ result
    return $ toNoProject issues

getIssue :: ProjectId -> IssueId -> Handler (Either ErrorReason (Entity Issue))
getIssue pId issueId = do
    result <- runIssueDB pId issueId $ \_ issue -> return issue
    return $ toNoProject result

deleteIssue :: ProjectId -> IssueId -> Handler (Either ErrorReason IssueId)
deleteIssue pId issueId = do
    result <- runIssueDB pId issueId $ \project _ -> do
        let issueIds = projectIssueIds $ entityVal project
        lift $ update pId [ ProjectIssueIds =. (L.delete issueId issueIds) ]
        lift $ delete issueId
        return issueId
    return $ toNoProject result

changeIssue :: ProjectId -> IssueId -> ChangeIssuePayload -> Handler (Either ErrorReason (Entity Issue))
changeIssue pId issueId ChangeIssuePayload { title = title, text = text} = do
    result <- runIssueDB pId issueId $ \_ _ -> do
        result <- lift $ updateGet issueId [ IssueTitle =. title, IssueText =. text ]
        return $ Entity issueId result
    return $ toNoProject result

runIssueDB :: ProjectId -> IssueId -> (Entity Project -> Entity Issue -> MaybeT (YesodDB App) a) -> Handler (Maybe a)
runIssueDB pId issueId action = runProjectIssuesDB pId $ \project -> do
    issue <- MaybeT $ getEntity issueId
    action project issue

runProjectIssuesDB :: ProjectId -> (Entity Project -> MaybeT (YesodDB App) a) -> Handler (Maybe a)
runProjectIssuesDB pId action = runDB $ runMaybeT $ do
    project <- findNotArchivedProject pId
    action project

toNoProject :: Maybe a -> Either ErrorReason a
toNoProject Nothing = Left NoProject
toNoProject (Just x) = Right x
