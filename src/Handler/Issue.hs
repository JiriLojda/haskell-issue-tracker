module Handler.Issue where

import Import
import Services.Issue
import RequestModels.Issue
import Handler.Utils

postNewIssueR :: ProjectId -> Handler Value
postNewIssueR pId = do
    issue <- (requireJsonBody :: Handler Issue)
    result <- createIssue pId issue
    createStatusResponse created201 result

getIssuesR :: ProjectId -> Handler Value
getIssuesR pId = do
    issues <- getAllIssues pId
    createResponse issues

getIssueR :: ProjectId -> IssueId -> Handler Value
getIssueR pId issueId = do
    issue <- getIssue pId issueId
    createResponse issue

deleteIssueR :: ProjectId -> IssueId -> Handler Value
deleteIssueR pId issueId = do
    result <- deleteIssue pId issueId
    createResponse result

patchIssueR :: ProjectId -> IssueId -> Handler Value
patchIssueR pId issueId = do
    payload <- (requireJsonBody :: Handler ChangeIssuePayload)
    modified <- changeIssue pId issueId payload
    createResponse modified
