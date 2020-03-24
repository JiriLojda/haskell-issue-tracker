module Services.IssueComment where

import Data.UUID.V4
import Data.UUID
import Control.Monad.Trans.Maybe
import Data.Map (toList)
import qualified Data.List as L

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

getAllComments :: ProjectId -> IssueId -> ServiceReturn [Entity IssueComment]
getAllComments pId issueId = do
    result <- runDB $ runMaybeT $ do
        project <- MaybeT $ getEntity pId
        issue <- MaybeT $ getEntity issueId
        comments <- lift $ getMany $ issueComments $ entityVal issue
        return . (map (uncurry Entity)) . toList $ comments
    return $ toNoProject result

updateComment :: ProjectId -> IssueId -> IssueCommentId -> IssueComment -> ServiceReturn (Entity IssueComment)
updateComment pId issueId commentId comment = do
    result <- runDB $ runMaybeT $ do
        _ <- MaybeT $ get pId
        _ <- MaybeT $ get issueId
        _ <- MaybeT $ get commentId
        lift $ replace commentId comment
        return $ Entity commentId comment
    return $ toNoProject result

getComment :: ProjectId -> IssueId -> IssueCommentId -> ServiceReturn (Entity IssueComment)
getComment pId issueId commentId = do
    result <- runDB $ runMaybeT $ do
        _ <- MaybeT $ get pId
        _ <- MaybeT $ get issueId
        comment <- MaybeT $ getEntity commentId
        return comment
    return $ toNoProject result


deleteComment :: ProjectId -> IssueId -> IssueCommentId -> ServiceReturn (IssueCommentId)
deleteComment pId issueId commentId = do
    result <- runDB $ runMaybeT $ do
        _ <- MaybeT $ get pId
        issue <- MaybeT $ getEntity issueId
        _ <- MaybeT $ get commentId
        lift $ delete commentId
        let existingCommentIds = issueComments $ entityVal issue
        lift $ update issueId [ IssueComments =. L.delete commentId existingCommentIds ]
        return commentId
    return $ toNoProject result

type ServiceReturn a = Handler (Either ErrorReason a)
