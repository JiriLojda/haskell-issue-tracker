module Repositories.Project where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe

import Import hiding ((.))
import SharedTypes

getProject :: ProjectId -> DBAction (Entity Project)
getProject = (maybeToExceptT NoProject) . MaybeT . getEntity

findNotArchivedProject :: ProjectId -> DBAction (Entity Project)
findNotArchivedProject pId = do
    project <- getProject pId
    if projectIsArchived $ entityVal project
        then throwE ArchivedProject
        else return project

updateProject :: ProjectId -> [Update Project] -> DBAction (Entity Project)
updateProject pId mods = do
    entity <- lift $ updateGet pId mods
    return $ Entity pId entity
