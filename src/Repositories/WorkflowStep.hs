module Repositories.WorkflowStep where

import Data.UUID.V4
import Data.UUID
import Control.Monad.Trans.Maybe

import Import hiding ((.))
import SharedTypes

getWorkflowStep :: WorkflowStepId -> DBAction (Entity WorkflowStep)
getWorkflowStep = (maybeToExceptT NoWorkflowStep) . MaybeT . getEntity

replaceWorkflowStep :: WorkflowStepId -> WorkflowStep -> DBAction (Entity WorkflowStep)
replaceWorkflowStep stepId step = do
    lift $ replace stepId step
    return $ Entity stepId step

createWorkflowStep :: WorkflowStep -> DBAction (Entity WorkflowStep)
createWorkflowStep step = do
    key <- liftIO nextRandom
    let id = WorkflowStepKey $ toText key
    lift $ insertKey id step
    return $ Entity id step
