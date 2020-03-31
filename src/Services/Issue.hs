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
import qualified RequestModels.Issue.Create as C
import qualified RequestModels.Issue.Change as M
import SharedTypes
import Repositories.Issue hiding (getIssue)

createIssue :: ProjectId -> C.CreateIssuePayload -> ServiceReturn (Entity Issue)
createIssue pId C.CreateIssuePayload { C.title = title, C.text = text, C.step = step } = do
    newId <- liftIO nextRandom
    let issueId = IssueKey $ toText $ newId
        issue = Issue title text [] step
    runProjectDB pId $ \project -> do
        lift $ insertKey issueId issue
        let existingIssueIds = projectIssueIds $ entityVal project
        lift $ update pId [ ProjectIssueIds =. (issueId : existingIssueIds) ]
        return $ Entity issueId issue

getAllIssues :: ProjectId -> ServiceReturn [Entity Issue]
getAllIssues pId = runProjectDB pId $ \project -> do
        result <- lift $ getMany $ projectIssueIds $ entityVal project
        return . (map (uncurry Entity)) . toList $ result

getIssue :: ProjectId -> IssueId -> ServiceReturn (Entity Issue)
getIssue pId issueId = runIssueDB pId issueId $ \_ issue -> return issue

deleteIssue :: ProjectId -> IssueId -> ServiceReturn IssueId
deleteIssue pId issueId = runIssueDB pId issueId $ \project _ -> do
        let issueIds = projectIssueIds $ entityVal project
        lift $ update pId [ ProjectIssueIds =. (L.delete issueId issueIds) ]
        lift $ delete issueId
        return issueId

changeIssue :: ProjectId -> IssueId -> M.ChangeIssuePayload -> ServiceReturn (Entity Issue)
changeIssue pId issueId M.ChangeIssuePayload { M.title = title, M.text = text} = 
    runIssueDB pId issueId $ \_ _ -> updateIssue issueId [ IssueTitle =. title, IssueText =. text ]
