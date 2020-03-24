module Handler.IssueComment where

import Import
import Services.IssueComment
import Services.Issue

postNewIssueCommentR :: ProjectId -> IssueId -> Handler Value
postNewIssueCommentR pId issueId = do
    payload <- (requireJsonBody :: Handler IssueComment)
    result <- createComment pId issueId payload
    case result of
        Left _ -> notFound
        Right x -> sendStatusJSON created201 x

getIssueCommentsR :: ProjectId -> IssueId -> Handler Value
getIssueCommentsR pId issueId = do
    result <- getAllComments pId issueId
    case result of
        Left _ -> notFound
        Right x -> returnJson x

getIssueCommentR :: ProjectId -> IssueId -> IssueCommentId -> Handler Value
getIssueCommentR pId issueId commentId = do
    result <- getComment pId issueId commentId
    getResponse result


deleteIssueCommentR :: ProjectId -> IssueId -> IssueCommentId -> Handler Value
deleteIssueCommentR pId issueId commentId = do
    result <- deleteComment pId issueId commentId
    getResponse result

putIssueCommentR :: ProjectId -> IssueId -> IssueCommentId -> Handler Value
putIssueCommentR pId issueId commentId = do
    payload <- (requireJsonBody :: Handler IssueComment)
    result <- updateComment pId issueId commentId payload
    getResponse result

getResponse :: (ToJSON b) => Either ErrorReason b -> Handler Value
getResponse (Left _) = notFound
getResponse (Right x) = returnJson x
