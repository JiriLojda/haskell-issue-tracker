{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module AppModel where

import Data.Cache (Cache)
import Yesod.Core.Types (Logger)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Database.Persist.Types (Entity)

import Model
import Settings (AppSettings)
import Import.NoFoundation

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , dataCache      :: DataCache
    }

data DataCache = DataCache { projectsCache :: Cache ProjectId (Entity Project) }


-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT (HandlerFor App) a -> (HandlerFor App) a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

