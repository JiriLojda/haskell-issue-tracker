{-# LANGUAGE DeriveGeneric #-}

module Handler.Project where

import Import hiding (putStrLn, (.))
import Data.UUID.V4
import Data.UUID
import Data.Text
import Control.Monad.Trans.Maybe
import Model

postNewProjectR :: Handler Value
postNewProjectR = do
    project <- (requireJsonBody :: Handler Project)
    id <- liftIO nextRandom
    insertedProject <- runDB $ insertKey (ProjectKey $ toText id) project
    sendStatusJSON created201  id

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
    project <- runDB $ delete pId
    sendStatusJSON ok200 ()

data RenameProjectPayload = RenameProjectPayload { name :: Text } deriving (Generic, Show)

instance FromJSON RenameProjectPayload where

patchRenameProjectR :: ProjectId -> Handler Value
patchRenameProjectR pId = do
    payload <- (requireJsonBody :: Handler RenameProjectPayload)
    updatedProject <- runDB $ runMaybeT $ renameProject pId $ name payload
    case updatedProject of
        Just x -> returnJson x
        Nothing -> notFound

renameProject :: ProjectId -> Text -> MaybeT (YesodDB App) (Entity Project)
renameProject pId newName = do
        originalProject <- MaybeT $ get $ pId
        updatedProject <- lift $ updateGet pId [ ProjectName =. newName ]
        return $ Entity pId updatedProject
