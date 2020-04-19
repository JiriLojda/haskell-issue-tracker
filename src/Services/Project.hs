{-# LANGUAGE OverloadedStrings #-}

module Services.Project 
(
    renameProject
    , unarchiveProject
    , archiveProject
    , createProject
    , getAllProjects
    , getProject
    , deleteProject
    , addContributor
    , removeContributor
) where

import Control.Monad.Trans.Except
import Data.UUID.V4
import Data.UUID
import Database.Persist.Sqlite
import qualified Data.List as List

import Import hiding ((.), id)
import SharedTypes
import qualified Repositories.Project as R
import Repositories.WorkflowStep
import Services.Utils
import Services.User

createProject :: Text -> ServiceReturn (Entity Project)
createProject name = do
    newId <- liftIO nextRandom
    userId <- currentUserId
    runDB $ runExceptT $ do
        wfStepIds <- insertDefaultWorkflow
        let pId = ProjectKey $ toText newId
            project = Project name False [] wfStepIds [userId]
        lift $ insertKey pId project
        return $ Entity pId project

getAllProjects :: ServiceReturn [Entity Project]
getAllProjects = do
    result <- runDB $ selectList [] [Asc ProjectId]
    return $ Right result 

getProject :: ProjectId -> ServiceReturn (Entity Project)
getProject = runDB . runExceptT . R.getProject

deleteProject :: ProjectId -> ServiceReturn ProjectId
deleteProject pId = runProjectDB pId $ const $ R.deleteProject pId

renameProject :: ProjectId -> Text -> ServiceReturn (Entity Project)
renameProject pId newName = runProjectDB pId $ \_ -> R.updateProject pId [ ProjectName =. newName ]

archiveProject :: ProjectId -> ServiceReturn (Entity Project)
archiveProject pId = runProjectDB pId $ \_ -> R.updateProject pId [ ProjectIsArchived =. True ]

unarchiveProject :: ProjectId -> ServiceReturn (Entity Project)
unarchiveProject pId = runAllProjectDB pId $ \_ -> R.updateProject pId [ ProjectIsArchived =. False ]

addContributor :: ProjectId -> UserId -> ServiceReturn (Entity Project)
addContributor pId userId = do
    user <- getUser userId
    runProjectDB pId $ \project -> do 
        let newContributorIds = (userId : (projectContributorIds $ entityVal project))
        R.updateProject pId [ ProjectContributorIds =. newContributorIds ]

removeContributor :: ProjectId -> UserId -> ServiceReturn (Entity Project)
removeContributor pId userId = do
    user <- getUser userId
    runProjectDB pId $ \project -> do 
        let oldContributorIds = projectContributorIds $ entityVal project
        R.updateProject pId [ ProjectContributorIds =. (List.delete userId oldContributorIds) ]

defaultWorkflow :: [WorkflowStep]
defaultWorkflow = [WorkflowStep "Open", WorkflowStep "Waiting", WorkflowStep "Closed"]

insertDefaultWorkflow :: DBAction [WorkflowStepId]
insertDefaultWorkflow = mapM ((>>= return . entityKey) . createWorkflowStep) defaultWorkflow

