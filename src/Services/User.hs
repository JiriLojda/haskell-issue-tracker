{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Services.User where

import Database.Persist.Types (Entity(..))
import Database.Persist (insertKey, getEntity, replace)
import Yesod.Persist.Core (runDB)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.Either.Utils

import Model
import SharedTypes

getUser :: UserId -> ServiceReturn (Entity User)
getUser userId = do
    result <- runDB $ getEntity userId
    return $ maybeToEither NoUser result

createUser :: UserId -> User -> ServiceReturn (Entity User)
createUser userId user = runDB $ do
    existing <- getEntity userId
    case existing of
        Just _ -> return $ Left UserAlreadyExists
        Nothing -> do
            insertKey userId user
            return $ Right $ Entity userId user

updateUser :: UserId -> User -> ServiceReturn (Entity User)
updateUser userId user = runExceptT $ do
    _ <- ExceptT $ getUser userId
    lift $ runDB $ replace userId user
    return $ Entity userId user
