{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Services.User where

import Yesod.Core.Handler (HandlerFor)
import Database.Persist.Types (Entity(..))
import Database.Persist (insertKey, getEntity, replace)
import Database.Persist.Sqlite (SqlBackend)
import Yesod.Persist.Core (YesodPersist, YesodPersistBackend, runDB)

import Model

getUser :: (YesodPersist s, YesodPersistBackend s ~ SqlBackend) => UserId -> HandlerFor s (Maybe (Entity User))
getUser = runDB . getEntity

createUser :: (YesodPersist s, YesodPersistBackend s ~ SqlBackend) => UserId -> User -> HandlerFor s (Maybe (Entity User))
createUser userId user = runDB $ do
    existing <- getEntity userId
    case existing of
        Just _ -> return Nothing
        Nothing -> do
            insertKey userId user
            return $ Just $ Entity userId user

updateUser :: (YesodPersist s, YesodPersistBackend s ~ SqlBackend) => UserId -> User -> HandlerFor s (Maybe (Entity User))
updateUser userId user = runDB $ do
    existing <- getEntity userId
    case existing of
        Nothing -> return Nothing
        Just _ -> do
            replace userId user
            return $ Just $ Entity userId user
