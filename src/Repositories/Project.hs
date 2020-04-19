module Repositories.Project (
    getProject
    , findNotArchivedProject
    , updateProject
    , deleteProject
) where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Database.Persist.Types (Entity(..), Update)
import Database.Persist.Class (delete, updateGet, getEntity)

import Model
import SharedTypes (DBAction, ErrorReason(..))
import AppModel (DataCache(..))
import Repositories.Utils

getProjectNotCached :: ProjectId -> DBAction (Entity Project)
getProjectNotCached = (maybeToExceptT NoProject) . MaybeT . getEntity

getProject :: ProjectId -> DBAction (Entity Project)
getProject pId = fetchWithCache projectsCache pId getProjectNotCached

findNotArchivedProject :: ProjectId -> DBAction (Entity Project)
findNotArchivedProject pId = do
    project <- getProject pId
    if projectIsArchived $ entityVal project
        then throwE ArchivedProject
        else return project

updateProject :: ProjectId -> [Update Project] -> DBAction (Entity Project)
updateProject pId mods = do
    removeFromCache projectsCache pId
    entity <- lift $ updateGet pId mods
    return $ Entity pId entity

deleteProject :: ProjectId -> DBAction ProjectId
deleteProject pId = do
    removeFromCache projectsCache pId
    lift $ delete pId
    return pId
