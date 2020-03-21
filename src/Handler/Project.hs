module Handler.Project where

import Import hiding (putStrLn, (.))
import Data.UUID.V4
import Data.UUID
import Model

postNewProjectR :: Handler Value
postNewProjectR = do
    project <- (requireJsonBody :: Handler Project)
    id <- liftIO nextRandom
    insertedProject <- runDB $ insertKey (ProjectKey $ toText id) project
    returnJson id

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
