module Handler.User where

import Import
import Services.User
import qualified RequestModels.User.Create as C
import qualified RequestModels.User.Update as U
import Handler.Utils

postSignupR :: Handler Value
postSignupR = do
    payload <- (requireJsonBody :: Handler C.CreateUserPayload)
    userId <- currentUserId
    result <- createUser userId $ C.toUser payload
    createStatusResponse created201 result

getUserR :: UserId -> Handler Value
getUserR userId = getUser userId >>= createResponse

postUserR :: UserId -> Handler Value
postUserR userId = do
    payload <- (requireJsonBody :: Handler U.UpdateUserPayload)
    result <- updateUser userId $ U.toUser payload
    createResponse result
