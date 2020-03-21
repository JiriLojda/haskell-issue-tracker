{-# LANGUAGE DeriveGeneric #-}

module Handler.Project where

import Import hiding (putStrLn, (.))
import Data.UUID.V4
import Data.UUID
import Control.Monad.Trans.Maybe

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

data RenameProjectPayload = RenameProjectPayload { name :: Text } deriving (Generic, Show)

instance FromJSON RenameProjectPayload where

patchRenameProjectR :: ProjectId -> Handler Value
patchRenameProjectR pId = do
    payload <- (requireJsonBody :: Handler RenameProjectPayload)
    updatedProject <- renameProject pId $ name payload
    case updatedProject of
        Just x -> returnJson x
        Nothing -> notFound

renameProject :: ProjectId -> Text -> Handler (Maybe (Entity Project))
renameProject pId newName = runDB $ runMaybeT $ updateProject pId [ ProjectName =. newName ]

archiveProject :: ProjectId -> Handler (Maybe (Entity Project))
archiveProject pId = runDB $ runMaybeT $ updateProject pId [ ProjectIsArchived =. True ]

unarchiveProject :: ProjectId -> Handler (Maybe (Entity Project))
unarchiveProject pId = runDB $ runMaybeT $ updateProject pId [ ProjectIsArchived =. False ]

updateProject :: ProjectId -> [Update Project] -> MaybeT (YesodDB App) (Entity Project)
updateProject pId mods = do
    _ <- MaybeT $ get $ pId
    updatedProject <- lift $ updateGet pId mods
    return $ Entity pId updatedProject

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
