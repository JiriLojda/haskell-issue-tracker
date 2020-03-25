module Handler.Utils where

import Import
import SharedTypes

createResponse :: (ToJSON b) => Either ErrorReason b -> Handler Value
createResponse (Left _) = notFound
createResponse (Right x) = returnJson x

createStatusResponse :: (ToJSON a) => Status -> Either ErrorReason a -> Handler Value
createStatusResponse _ (Left _) = notFound
createStatusResponse status (Right x) = sendStatusJSON status x
