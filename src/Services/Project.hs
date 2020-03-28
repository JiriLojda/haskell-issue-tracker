module Services.Project 
(
    renameProject
    , unarchiveProject
    , archiveProject
    , createProject
    , getAllProjects
    , getProject
    , deleteProject
) where

import Control.Monad.Trans.Except
import Data.UUID.V4
import Data.UUID

import Import hiding ((.), id)
import SharedTypes
import qualified Repositories.Project as R
import Services.Utils

createProject :: Project -> ServiceReturn (Entity Project)
createProject project = do
    newId <- liftIO nextRandom
    let pId = ProjectKey $ toText newId
    runDB $ insertKey pId project
    return $ Right $ Entity pId project

getAllProjects :: ServiceReturn [Entity Project]
getAllProjects = do
    result <- runDB $ selectList [] [Asc ProjectId]
    return $ Right result 

getProject :: ProjectId -> ServiceReturn (Entity Project)
getProject = runDB . runExceptT . R.getProject

deleteProject :: ProjectId -> ServiceReturn ProjectId
deleteProject pId = runProjectDB pId $ \_ -> do
    lift $ delete pId
    return pId

renameProject :: ProjectId -> Text -> ServiceReturn (Entity Project)
renameProject pId newName = runProjectDB pId $ \_ -> R.updateProject pId [ ProjectName =. newName ]

archiveProject :: ProjectId -> ServiceReturn (Entity Project)
archiveProject pId = runProjectDB pId $ \_ -> R.updateProject pId [ ProjectIsArchived =. True ]

unarchiveProject :: ProjectId -> ServiceReturn (Entity Project)
unarchiveProject pId = runAllProjectDB pId $ \_ -> R.updateProject pId [ ProjectIsArchived =. False ]
