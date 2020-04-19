module Handler.Project where

import Import hiding (putStrLn, (.))
import Services.Project
import Handler.Utils
import qualified RequestModels.Project.Create as C
import qualified RequestModels.Project.Rename as R

postNewProjectR :: Handler Value
postNewProjectR = do
    payload <- (requireJsonBody :: Handler C.CreateProjectPayload)
    result <- createProject $ C.name payload
    createStatusResponse created201 result

getProjectsR :: Handler Value
getProjectsR = getAllProjects >>= createResponse

getProjectR :: ProjectId -> Handler Value
getProjectR pId = getProject pId >>= createResponse

deleteProjectR :: ProjectId -> Handler Value
deleteProjectR pId = deleteProject pId >>= createResponse

patchRenameProjectR :: ProjectId -> Handler Value
patchRenameProjectR pId = do
    payload <- (requireJsonBody :: Handler R.RenameProjectPayload)
    updatedProject <- renameProject pId $ R.name payload
    createResponse updatedProject

patchArchiveProjectR :: ProjectId -> Handler Value
patchArchiveProjectR pId = archiveProject pId >>= createResponse

patchUnarchiveProjectR :: ProjectId -> Handler Value
patchUnarchiveProjectR pId = unarchiveProject pId >>= createResponse

postProjectContributorR :: ProjectId -> UserId -> Handler Value
postProjectContributorR pId userId = addContributor pId userId >>= createResponse

deleteProjectContributorR :: ProjectId -> UserId -> Handler Value
deleteProjectContributorR pId userId = removeContributor pId userId >>= createResponse
