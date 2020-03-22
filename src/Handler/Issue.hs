module Handler.Issue where

import Import
import Services.Issue

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
    case issues of
        Left _ -> notFound
        Right x -> returnJson x
