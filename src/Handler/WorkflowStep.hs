module Handler.WorkflowStep where

import Import
import Services.WorkflowStep
import Handler.Utils

postAllWorkflowR :: ProjectId -> Handler Value
postAllWorkflowR pId = do
    payload <- (requireJsonBody :: Handler WorkflowStep)
    result <- createWorkflowStep pId payload
    createStatusResponse created201 result

getAllWorkflowR :: ProjectId -> Handler Value
getAllWorkflowR pId = do
    result <- getAllWorkflowSteps pId
    createResponse result

getWorkflowR :: ProjectId -> WorkflowStepId -> Handler Value
getWorkflowR pId stepId = do
    result <- findWorkflowStep pId stepId
    createResponse result

putWorkflowR :: ProjectId -> WorkflowStepId -> Handler Value
putWorkflowR pId stepId = do
    payload <- (requireJsonBody :: Handler WorkflowStep)
    result <- changeWorkflowStep pId stepId payload
    createResponse result

deleteWorkflowR :: ProjectId -> WorkflowStepId -> Handler Value
deleteWorkflowR pId stepId = do
    result <- deleteWorkflowStep pId stepId
    createResponse result
