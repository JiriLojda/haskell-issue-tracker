module Services.Project 
(renameProject
, unarchiveProject
, archiveProject
) where

import Control.Monad.Trans.Except

import Import
import SharedTypes
import Repositories.Project
import Services.Utils

renameProject :: ProjectId -> Text -> ServiceReturn (Entity Project)
renameProject pId newName = runProjectDB pId $ \_ -> updateProject pId [ ProjectName =. newName ]

archiveProject :: ProjectId -> ServiceReturn (Entity Project)
archiveProject pId = runProjectDB pId $ \_ -> updateProject pId [ ProjectIsArchived =. True ]

unarchiveProject :: ProjectId -> ServiceReturn (Entity Project)
unarchiveProject pId = runAllProjectDB pId $ \_ -> updateProject pId [ ProjectIsArchived =. False ]
