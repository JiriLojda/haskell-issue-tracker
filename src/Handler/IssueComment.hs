module Handler.IssueComment where

import Import
import Services.IssueComment

postNewIssueCommentR :: ProjectId -> IssueId -> Handler Value
postNewIssueCommentR pId issueId = do
    payload <- (requireJsonBody :: Handler IssueComment)
    result <- createComment pId issueId payload
    case result of
        Left _ -> notFound
        Right x -> sendStatusJSON created201 x

getIssueCommentsR :: ProjectId -> IssueId -> Handler Value
getIssueCommentsR pId issueId = do
    result <- getAllIssues pId issueId
    case result of
        Left _ -> notFound
        Right x -> returnJson x
