module Handler.Project where

import Import hiding (putStrLn, (.))
import Services.Project
import Handler.Utils
import RequestModels.Project

postNewProjectR :: Handler Value
postNewProjectR = do
    project <- (requireJsonBody :: Handler Project)
    result <- createProject project
    createStatusResponse created201 result

getProjectsR :: Handler Value
getProjectsR = getAllProjects >>= createResponse

getProjectR :: ProjectId -> Handler Value
getProjectR pId = getProject pId >>= createResponse

deleteProjectR :: ProjectId -> Handler Value
deleteProjectR pId = deleteProject pId >>= createResponse

patchRenameProjectR :: ProjectId -> Handler Value
patchRenameProjectR pId = do
    payload <- (requireJsonBody :: Handler RenameProjectPayload)
    updatedProject <- renameProject pId $ name payload
    createResponse updatedProject

patchArchiveProjectR :: ProjectId -> Handler Value
patchArchiveProjectR pId = do
    updatedProject <- archiveProject pId
    createResponse updatedProject

patchUnarchiveProjectR :: ProjectId -> Handler Value
patchUnarchiveProjectR pId = do
    updatedProject <- unarchiveProject pId
    createResponse updatedProject
