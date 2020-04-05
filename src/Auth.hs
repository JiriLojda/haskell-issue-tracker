{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Auth (verifyUserToken, createErrorMessage) where
import Import.NoFoundation hiding (keys)
import Crypto.JOSE.JWK (JWK)
import Crypto.JOSE.Error (Error(..))
import Crypto.JWT (
    JWTError(..)
    , JWTValidationSettings
    , defaultJWTValidationSettings
    , jwtValidationSettingsIssuerPredicate
    , verifyClaims
    , claimSub
    , decodeCompact
    )
import Network.HTTP.Simple (JSONException, httpJSONEither, getResponseBody)
import qualified Data.ByteString.Lazy.Internal as B
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import qualified Data.List as Lst
import Control.Lens (over, view)

data KeyResponse = KeyResponse{ keys :: [JWK] } deriving (Show, Generic)
instance FromJSON KeyResponse where

verifyUserToken :: AppSettings -> B.ByteString -> IO (Either JWTError String)
verifyUserToken settings token = runExceptT $ do
    maybeJwk <- lift $ loadPublicKey $ appAuthPublicKeyRoute settings
    justJwk <- ExceptT $ return $ case maybeJwk of
        Nothing -> Left $ JWTClaimsSetDecodeError "Internal error while validating token."
        Just x -> Right x
    decodedToken <- decodeCompact token
    claims <- verifyClaims (createJwtValidationConfig settings) justJwk decodedToken
    -- consider checking no unregistered claims:
    -- when (not $ null $ view unregisteredClaims claims) (ExceptT $ return $ Left $ JWTClaimsSetDecodeError ("Unregistered claims encountered: " ++ (show $ view unregisteredClaims claims)))
    return $ show $ view claimSub claims

createErrorMessage :: JWTError -> Text
createErrorMessage JWTExpired = "The token is expired."
createErrorMessage (JWTClaimsSetDecodeError e) = ("Invalid claims " ++ pack e)
createErrorMessage JWTNotInIssuer = "Bad issuer."
createErrorMessage JWTNotInAudience = "Bad audience."
createErrorMessage JWTIssuedAtFuture = "Token is not valid now."
createErrorMessage JWTNotYetValid = "Token is not valid now."
createErrorMessage (JWSError e) = createJwsErrorMessage e
createErrorMessage _ = "Token is not valid."

createJwsErrorMessage :: Error -> Text
createJwsErrorMessage AlgorithmNotImplemented = "Algorithm not supported."
createJwsErrorMessage (AlgorithmMismatch _) = "Algorithm not supported."
createJwsErrorMessage (KeyMismatch e) = "Wrong type of key encountered: " ++ (pack e)
createJwsErrorMessage KeySizeTooSmall = "Too small key encountered."
createJwsErrorMessage (JSONDecodeError _) = "Error while decoding JSON."
createJwsErrorMessage _ = "The token is invalid."

createJwtValidationConfig :: AppSettings -> JWTValidationSettings
createJwtValidationConfig settings = over
    jwtValidationSettingsIssuerPredicate
    (const (== (fromString . appAuthIssuer) settings)) $
    defaultJWTValidationSettings (== (fromString . appAuthAudience) settings)

loadPublicKey :: String -> IO (Maybe JWK)
loadPublicKey url = do
    request <- parseRequest url
    response <- (httpJSONEither request :: IO (Response (Either JSONException KeyResponse)))
    return $ case getResponseBody response of
        Left _ -> Nothing
        Right x -> Just $ Lst.head $ keys x
