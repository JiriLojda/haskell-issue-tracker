module Handler.IssueComment where

import Import
import Services.IssueComment
import Handler.Utils

postNewIssueCommentR :: ProjectId -> IssueId -> Handler Value
postNewIssueCommentR pId issueId = do
    payload <- (requireJsonBody :: Handler IssueComment)
    result <- createComment pId issueId payload
    createStatusResponse created201 result

getIssueCommentsR :: ProjectId -> IssueId -> Handler Value
getIssueCommentsR pId issueId = getAllComments pId issueId >>= createResponse

getIssueCommentR :: ProjectId -> IssueId -> IssueCommentId -> Handler Value
getIssueCommentR pId issueId commentId = getComment pId issueId commentId >>= createResponse

deleteIssueCommentR :: ProjectId -> IssueId -> IssueCommentId -> Handler Value
deleteIssueCommentR pId issueId commentId = deleteComment pId issueId commentId >>= createResponse

putIssueCommentR :: ProjectId -> IssueId -> IssueCommentId -> Handler Value
putIssueCommentR pId issueId commentId = do
    payload <- (requireJsonBody :: Handler IssueComment)
    result <- updateComment pId issueId commentId payload
    createResponse result