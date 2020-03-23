module Handler.Issue where

import Import
import Services.Issue
import RequestModels.Issue

postNewIssueR :: ProjectId -> Handler Value
postNewIssueR pId = do
    issue <- (requireJsonBody :: Handler Issue)
    result <- createIssue pId issue
    case result of
        Left _ -> notFound
        Right x -> sendStatusJSON created201 x

getIssuesR :: ProjectId -> Handler Value
getIssuesR pId = do
    issues <- getAllIssues pId
    getResponse issues

getIssueR :: ProjectId -> IssueId -> Handler Value
getIssueR pId issueId = do
    issue <- getIssue pId issueId
    getResponse issue

deleteIssueR :: ProjectId -> IssueId -> Handler Value
deleteIssueR pId issueId = do
    result <- deleteIssue pId issueId
    getResponse result

patchIssueR :: ProjectId -> IssueId -> Handler Value
patchIssueR pId issueId = do
    payload <- (requireJsonBody :: Handler ChangeIssuePayload)
    modified <- changeIssue pId issueId payload
    getResponse modified

getResponse :: (ToJSON b) => Either ErrorReason b -> Handler Value
getResponse (Left _) = notFound
getResponse (Right x) = returnJson x
