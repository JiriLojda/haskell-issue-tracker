module Handler.Project where

import Data.UUID.V4
import Data.UUID

import Import hiding (putStrLn, (.))
import Services.Project
import RequestModels.Project

postNewProjectR :: Handler Value
postNewProjectR = do
    project <- (requireJsonBody :: Handler Project)
    newId <- liftIO nextRandom
    let pId = ProjectKey $ toText newId
    runDB $ insertKey pId project
    sendStatusJSON created201 $ Entity pId project

getProjectsR :: Handler Value
getProjectsR = do
    projects <- runDB $ selectList [] [Asc ProjectId]
    returnJson projects

getProjectR :: ProjectId -> Handler Value
getProjectR pId = do
    project <- runDB $ get pId
    case project of
        Just x -> returnJson x
        Nothing -> notFound

deleteProjectR :: ProjectId -> Handler Value
deleteProjectR pId = do
    runDB $ delete pId
    sendStatusJSON ok200 ()

patchRenameProjectR :: ProjectId -> Handler Value
patchRenameProjectR pId = do
    payload <- (requireJsonBody :: Handler RenameProjectPayload)
    updatedProject <- renameProject pId $ name payload
    case updatedProject of
        Just x -> returnJson x
        Nothing -> notFound

patchArchiveProjectR :: ProjectId -> Handler Value
patchArchiveProjectR pId = do
    updatedProject <- archiveProject pId
    case updatedProject of
        Just x -> returnJson x
        Nothing -> notFound

patchUnarchiveProjectR :: ProjectId -> Handler Value
patchUnarchiveProjectR pId = do
    updatedProject <- unarchiveProject pId
    case updatedProject of
        Just x -> returnJson x
        Nothing -> notFound
