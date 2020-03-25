module Services.Issue
(
    createIssue
    , getAllIssues
    , changeIssue
    , deleteIssue
    , getIssue
) where

import Data.UUID.V4
import Data.UUID
import qualified Data.List as L
import Data.Map (toList)

import Import hiding (map, (.), toList)
import Services.Utils
import RequestModels.Issue
import SharedTypes

createIssue :: ProjectId -> Issue -> ServiceReturn (Entity Issue)
createIssue pId issue = do
    newId <- liftIO nextRandom
    let issueId = IssueKey $ toText $ newId
    result <- runProjectDB pId $ \project -> do
        lift $ insertKey issueId issue
        let existingIssueIds = projectIssueIds $ entityVal project
        lift $ update pId [ ProjectIssueIds =. (issueId : existingIssueIds) ]
        return $ Entity issueId issue
    return $ toNoProject result

getAllIssues :: ProjectId -> ServiceReturn [Entity Issue]
getAllIssues pId = do
    issues <- runProjectDB pId $ \project -> do
        result <- lift $ getMany $ projectIssueIds $ entityVal project
        return . (map (uncurry Entity)) . toList $ result
    return $ toNoProject issues

getIssue :: ProjectId -> IssueId -> ServiceReturn (Entity Issue)
getIssue pId issueId = do
    result <- runIssueDB pId issueId $ \_ issue -> return issue
    return $ toNoProject result

deleteIssue :: ProjectId -> IssueId -> ServiceReturn IssueId
deleteIssue pId issueId = do
    result <- runIssueDB pId issueId $ \project _ -> do
        let issueIds = projectIssueIds $ entityVal project
        lift $ update pId [ ProjectIssueIds =. (L.delete issueId issueIds) ]
        lift $ delete issueId
        return issueId
    return $ toNoProject result

changeIssue :: ProjectId -> IssueId -> ChangeIssuePayload -> ServiceReturn (Entity Issue)
changeIssue pId issueId ChangeIssuePayload { title = title, text = text} = do
    result <- runIssueDB pId issueId $ \_ _ -> do
        result <- lift $ updateGet issueId [ IssueTitle =. title, IssueText =. text ]
        return $ Entity issueId result
    return $ toNoProject result
