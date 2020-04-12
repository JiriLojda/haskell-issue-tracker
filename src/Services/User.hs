{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Services.User where

import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Class
import Data.Text (Text)
import Yesod.Core.Handler (HandlerFor)
import Database.Persist.Types (Entity(..))
import Database.Persist (insertKey, getEntity)
import Database.Persist.Sqlite (SqlBackend)
import Yesod.Persist.Core (YesodPersist, YesodPersistBackend, runDB)

import Model

getUser :: (YesodPersist s, YesodPersistBackend s ~ SqlBackend) => UserId -> HandlerFor s (Maybe (Entity User))
getUser = runDB . getEntity

createUser :: (YesodPersist s, YesodPersistBackend s ~ SqlBackend) => Text -> User -> HandlerFor s (Maybe (Entity User))
createUser auth0Id user = runDB $ runMaybeT $ do
    let userId = UserKey auth0Id
    lift $ insertKey userId user
    return $ Entity userId user
