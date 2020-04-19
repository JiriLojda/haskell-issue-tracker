{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Repositories.Utils where

import Database.Persist.Class (PersistRecordBackend)
import Database.Persist.Types (Key, Entity)
import Database.Persist.Sql (SqlBackend)
import qualified Data.Cache as C 
import Control.Monad.IO.Class (liftIO)
import Data.Hashable (Hashable)
import Yesod.Core.Handler (getYesod)

import SharedTypes
import AppModel (DataCache, dataCache)

type CacheSelector record = (Hashable (Key record), PersistRecordBackend record SqlBackend) => DataCache -> C.Cache (Key record) (Entity record)

fetchWithCache :: (Hashable (Key record), PersistRecordBackend record SqlBackend) => CacheSelector record -> Key record -> (Key record -> DBAction (Entity record)) -> DBAction (Entity record)
fetchWithCache selector eId getter = do
    cache <- fmap (selector . dataCache) getYesod
    found <- liftIO $ C.lookup cache eId
    case found of
        Just x -> return x
        Nothing -> do
            result <- getter eId
            liftIO $ C.insert cache eId result
            return result

removeFromCache :: (Hashable (Key record), PersistRecordBackend record SqlBackend) => CacheSelector record -> Key record -> DBAction ()
removeFromCache selector eId = do
    cache <- fmap (selector . dataCache) getYesod
    liftIO $ C.delete cache eId
