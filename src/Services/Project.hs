module Services.Project (renameProject, unarchiveProject, archiveProject) where

import Control.Monad.Trans.Maybe

import Import

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