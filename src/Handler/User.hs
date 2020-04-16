module Handler.User where

import Import
import Services.User
import qualified RequestModels.User.Create as C
import qualified RequestModels.User.Update as U

postSignupR :: Handler Value
postSignupR = do
    payload <- (requireJsonBody :: Handler C.CreateUserPayload)
    userId <- currentUserId
    result <- createUser userId $ C.toUser payload
    case result of
        Nothing -> sendStatusJSON badRequest400 "Failed to create new user."
        Just x -> sendStatusJSON created201 x

getUserR :: UserId -> Handler Value
getUserR userId = do
    result <- getUser userId
    case result of
        Nothing -> notFound
        Just x -> returnJson x

postUserR :: UserId -> Handler Value
postUserR userId = do
    payload <- (requireJsonBody :: Handler U.UpdateUserPayload)
    result <- updateUser userId $ U.toUser payload
    case result of
        Nothing -> sendStatusJSON badRequest400 "Failed to update the given user."
        Just x -> returnJson x
