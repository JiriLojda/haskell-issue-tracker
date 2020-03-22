module Services.Issue
(
    createIssue
    , getAllIssues
) where

import Data.UUID.V4
import Data.UUID
import Data.Map (toList)
import Control.Monad.Trans.Maybe

import Import hiding (map, (.), toList)
import Services.Project

data ErrorReason = NoProject | NoIssue

createIssue :: ProjectId -> Issue -> Handler (Either ErrorReason (Entity Issue))
createIssue pId issue = do
    newId <- liftIO nextRandom
    let issueId = IssueKey $ toText $ newId
    result <- runDB $ runMaybeT $ do
        project <- findNotArchivedProject pId
        lift $ insertKey issueId issue
        let existingIssueIds = projectIssueIds $ entityVal project
        lift $ update pId [ ProjectIssueIds =. (issueId : existingIssueIds) ]
        return $ Entity issueId issue
    return $ toNoProject result

getAllIssues :: ProjectId -> Handler (Either ErrorReason ([Entity Issue]))
getAllIssues pId = do
    issues <- runDB $ runMaybeT $ do
        project <- findNotArchivedProject pId
        result <- lift $ getMany $ projectIssueIds $ entityVal project
        return . (map (uncurry Entity)) . toList $ result
    return $ toNoProject issues

toNoProject :: Maybe a -> Either ErrorReason a
toNoProject Nothing = Left NoProject
toNoProject (Just x) = Right x
