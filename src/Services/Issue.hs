module Services.Issue where

import Data.UUID.V4
import Data.UUID
import Data.Either
import Data.Map (toList)
import Data.Maybe (fromJust)
import Control.Monad.Trans.Maybe

import Import hiding (map, (.), toList)
import Services.Project

data ErrorReason = NoProject | NoIssue

createIssue :: ProjectId -> Issue -> Handler (Either ErrorReason (Entity Issue))
createIssue pId issue = do
    newId <- liftIO nextRandom
    let issueId = IssueKey $ toText $ newId
        issueEntity = Entity issueId issue
    result <- runDB $ runMaybeT $ do
        project <- findNotArchivedProject pId
        inserted <- lift $ insertKey issueId issue
        let existingIssueIds = projectIssueIds $ entityVal $ fromJust $ project
        lift $ update pId [ ProjectIssueIds =. (issueId : existingIssueIds) ]
        return inserted
    case result of
        Nothing -> return $ Left NoProject
        Just x -> return $ Right issueEntity

toNoProject :: Maybe a -> Either ErrorReason a
toNoProject Nothing = Left NoProject
toNoProject (Just x) = Right x

getAllIssues :: ProjectId -> Handler (Either ErrorReason ([Entity Issue]))
getAllIssues pId = do
    issues <- runDB $ runMaybeT $ getMaybeIssues pId
    case issues of
        Nothing -> return $ Left NoProject
        Just x -> return $ Right x

getMaybeIssues :: ProjectId -> MaybeT (YesodDB App) ([Entity Issue])
getMaybeIssues pId = do
        project <- findNotArchivedProject pId
        result <- lift $ getMany $ projectIssueIds $ entityVal $ fromJust project
        return . (map (uncurry Entity)) . toList $ result
