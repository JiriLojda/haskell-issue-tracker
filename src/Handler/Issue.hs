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
getIssuesR pId = getAllIssues pId >>= createResponse

getIssueR :: ProjectId -> IssueId -> Handler Value
getIssueR pId issueId = getIssue pId issueId >>= createResponse

deleteIssueR :: ProjectId -> IssueId -> Handler Value
deleteIssueR pId issueId = deleteIssue pId issueId >>= createResponse

patchIssueR :: ProjectId -> IssueId -> Handler Value
patchIssueR pId issueId = do
    payload <- (requireJsonBody :: Handler ChangeIssuePayload)
    modified <- changeIssue pId issueId payload
    createResponse modified
