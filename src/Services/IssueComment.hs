module Services.IssueComment where

import Data.UUID.V4
import Data.UUID
import Control.Monad.Trans.Maybe
import Data.Map (toList)
import qualified Data.List as L

import Import hiding ((.), map, toList)
import Services.Issue

createComment :: ProjectId -> IssueId -> IssueComment -> ServiceReturn (Entity IssueComment)
createComment pId issueId comment = do
    newId <- liftIO $ nextRandom
    let key = IssueCommentKey $ toText newId
    result <- runDB $ runMaybeT $ do
        _ <- MaybeT $ getEntity pId
        issue <- MaybeT $ getEntity issueId
        lift $ insertKey key comment
        let existingCommentIds = issueComments $ entityVal $ issue
        lift $ update issueId [ IssueComments =. (key : existingCommentIds) ]
        return $ Entity key comment
    return $ toNoProject result

getAllComments :: ProjectId -> IssueId -> ServiceReturn [Entity IssueComment]
getAllComments pId issueId = do
    result <- runDB $ runMaybeT $ do
        _ <- MaybeT $ getEntity pId
        issue <- MaybeT $ getEntity issueId
        comments <- lift $ getMany $ issueComments $ entityVal issue
        return . (map (uncurry Entity)) . toList $ comments
    return $ toNoProject result

updateComment :: ProjectId -> IssueId -> IssueCommentId -> IssueComment -> ServiceReturn (Entity IssueComment)
updateComment pId issueId commentId comment = do
    result <- runCommentDB pId issueId commentId $ \_ _ _ -> do
        lift $ replace commentId comment
        return $ Entity commentId comment
    return $ toNoProject result

getComment :: ProjectId -> IssueId -> IssueCommentId -> ServiceReturn (Entity IssueComment)
getComment pId issueId commentId = do
    result <- runCommentDB pId issueId commentId $ \_ _ comment -> return comment
    return $ toNoProject result

deleteComment :: ProjectId -> IssueId -> IssueCommentId -> ServiceReturn (IssueCommentId)
deleteComment pId issueId commentId = do
    result <- runCommentDB pId issueId commentId $ \_ issue _ -> do
        lift $ delete commentId
        let existingCommentIds = issueComments $ entityVal issue
        lift $ update issueId [ IssueComments =. L.delete commentId existingCommentIds ]
        return commentId
    return $ toNoProject result

runCommentDB :: ProjectId -> IssueId -> IssueCommentId -> (E Project -> E Issue -> E IssueComment -> DBAction a) -> Handler (Maybe a)
runCommentDB pId issueId commentId action = runDB $ runMaybeT $ do
    project <- MaybeT $ getEntity pId
    issue <- MaybeT $ getEntity issueId
    comment <- MaybeT $ getEntity commentId
    action project issue comment

type ServiceReturn a = Handler (Either ErrorReason a)
type DBAction a = MaybeT (YesodDB App) a
type E a = Entity a
