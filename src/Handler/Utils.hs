module Handler.Utils where

import Import
import SharedTypes

createResponse :: (ToJSON b) => Either ErrorReason b -> Handler Value
createResponse (Right x) = returnJson x
createResponse (Left e) = createErrorResponse e

createStatusResponse :: (ToJSON a) => Status -> Either ErrorReason a -> Handler Value
createStatusResponse status (Right x) = sendStatusJSON status x
createStatusResponse _ (Left e) = createErrorResponse e

createErrorResponse :: ErrorReason -> Handler Value
createErrorResponse NoProject = sendStatusJSON status404 "Project not found."
createErrorResponse ArchivedProject = sendStatusJSON status404 "The project is archived."
createErrorResponse NoIssue = sendStatusJSON status404 "Issue not found."
createErrorResponse NoIssueComment = sendStatusJSON status404 "Comment not found."
