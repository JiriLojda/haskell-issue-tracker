module Services.WorkflowStep where

import Data.Map (toList)

import Import hiding ((.), toList, map)
import SharedTypes
import Services.Utils
import qualified Repositories.WorkflowStep as R

getAllWorkflowSteps :: ProjectId -> ServiceReturn [Entity WorkflowStep]
getAllWorkflowSteps pId = runProjectDB pId $ \project -> do
        result <- lift $ getMany $ projectWorkflowStepIds $ entityVal project
        return . (map (uncurry Entity)) . toList $ result

createWorkflowStep :: ProjectId -> WorkflowStep -> ServiceReturn (Entity WorkflowStep)
createWorkflowStep pId step = runProjectDB pId $ \project -> do
        newStep <- R.createWorkflowStep step
        let existingStepIds = projectWorkflowStepIds $ entityVal project
        lift $ update pId [ ProjectWorkflowStepIds =. (entityKey newStep : existingStepIds) ]
        return newStep
    
findWorkflowStep :: ProjectId -> WorkflowStepId -> ServiceReturn (Entity WorkflowStep)
findWorkflowStep pId stepId = runWorkflowStepDB pId stepId $ (return .) . (flip const)

changeWorkflowStep :: ProjectId -> WorkflowStepId -> WorkflowStep -> ServiceReturn (Entity WorkflowStep)
changeWorkflowStep pId stepId step = runWorkflowStepDB pId stepId $ \_ _ -> R.replaceWorkflowStep stepId step

deleteWorkflowStep :: ProjectId -> WorkflowStepId -> ServiceReturn WorkflowStepId
deleteWorkflowStep pId stepId = runWorkflowStepDB pId stepId $ \_ _ -> do
    lift $ delete stepId
    return stepId
