module Services.IssueComment where

import Data.UUID.V4
import Data.UUID
import Control.Monad.Trans.Maybe
import Data.Map (toList)

import Import hiding ((.), map, toList)
import Services.Project
import Services.Issue

createComment :: ProjectId -> IssueId -> IssueComment -> ServiceReturn (Entity IssueComment)
createComment pId issueId comment = do
    newId <- liftIO $ nextRandom
    let key = IssueCommentKey $ toText newId
    result <- runDB $ runMaybeT $ do
        project <- MaybeT $ getEntity pId
        issue <- MaybeT $ getEntity issueId
        lift $ insertKey key comment
        let existingCommentIds = issueComments $ entityVal $ issue
        lift $ update issueId [ IssueComments =. (key : existingCommentIds) ]
        return $ Entity key comment
    return $ toNoProject result

getAllIssues :: ProjectId -> IssueId -> ServiceReturn [Entity IssueComment]
getAllIssues pId issueId = do
    result <- runDB $ runMaybeT $ do
        project <- MaybeT $ getEntity pId
        issue <- MaybeT $ getEntity issueId
        comments <- lift $ getMany $ issueComments $ entityVal issue
        return . (map (uncurry Entity)) . toList $ comments
    return $ toNoProject result

type ServiceReturn a = Handler (Either ErrorReason a)
