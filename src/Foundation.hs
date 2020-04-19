{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foundation where

import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)
import Control.Monad.Trans.Except (runExceptT)

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

import Data.ByteString.Lazy (fromStrict)
import Data.ByteString (drop, length)
import qualified Network.Wai.Internal as W

import Import.NoFoundation hiding (fromStrict, keys, drop, putStrLn, length)
import Auth
import Services.User (getUser)
import Repositories.Project (getProject)
import AppModel

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root
    
    errorHandler NotFound = return $ toTypedContent ("The resource you were looking for was not found." :: String)
    errorHandler (InternalError reason) = return $ toTypedContent reason
    errorHandler (InvalidArgs names) = return $ toTypedContent $ "You provided invalid arguments: " <> (intercalate ", " names)
    errorHandler (PermissionDenied reason) = return $ toTypedContent $ "You don't have permission to do this. " <> reason
    errorHandler other = defaultErrorHandler other

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized

    -- Signup has to have a valid token, but no user is in a database yet
    isAuthorized SignupR _ = canCreateUser
    -- Routes that are authenticated with project access
    isAuthorized (ProjectR pId) _ = isAuthenticatedWithProject pId
    isAuthorized (UnarchiveProjectR pId) _ = isAuthenticatedWithProject pId
    isAuthorized (ArchiveProjectR pId) _ = isAuthenticatedWithProject pId
    isAuthorized (RenameProjectR pId) _ = isAuthenticatedWithProject pId
    isAuthorized (IssuesR pId) _ = isAuthenticatedWithProject pId
    isAuthorized (NewIssueR pId) _ = isAuthenticatedWithProject pId
    isAuthorized (AllWorkflowR pId) _ = isAuthenticatedWithProject pId
    isAuthorized (WorkflowR pId _) _ = isAuthenticatedWithProject pId
    isAuthorized (ProjectContributorR pId _) _ = isAuthenticatedWithProject pId
    isAuthorized (IssueR pId _) _ = isAuthenticatedWithProject pId
    isAuthorized (IssueCommentsR pId _) _ = isAuthenticatedWithProject pId
    isAuthorized (NewIssueCommentR pId _) _ = isAuthenticatedWithProject pId
    isAuthorized (IssueCommentR pId _ _) _ = isAuthenticatedWithProject pId
    -- Routes that are authenticated without project access
    isAuthorized NewProjectR _ = isAuthenticated
    isAuthorized ProjectsR _ = isAuthenticated
    isAuthorized (UserR _) _ = isAuthenticated

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

canCreateUser :: Handler AuthResult
canCreateUser = cachedEitherUserId >>= (return . either (Unauthorized . pack) (const Authorized))

isAuthenticated :: Handler AuthResult
isAuthenticated = cachedEitherUser >>= (return . either (Unauthorized . pack) (const Authorized))

isAuthenticatedWithProject :: ProjectId -> Handler AuthResult
isAuthenticatedWithProject pId = do
    baseAuthResult <- isAuthenticated
    case baseAuthResult of
        Unauthorized e -> return $ Unauthorized e
        AuthenticationRequired -> return $ Unauthorized "You have to authenticate first."
        Authorized -> do
            canAccessProject <- canCurrentUserAccessProject pId
            return $ if canAccessProject
                then Authorized
                else Unauthorized "You are not a contributor of the project."

currentUserId :: Handler UserId
currentUserId = cachedEitherUserId >>= (return . either (const $ error "User is not authorized.") id)

currentUser :: Handler (Entity User)
currentUser = cachedEitherUser >>= (return . either (const $ error "User is not authorized.") id)

newtype UserIdCache = UserIdCache { eitherUserId :: Either String UserId }
newtype UserCache = UserCache { eitherUser :: Either String (Entity User) }

cachedEitherUserId :: Handler (Either String UserId)
cachedEitherUserId = fmap eitherUserId $ cached $ fmap UserIdCache getEitherUserId

cachedEitherUser :: Handler (Either String (Entity User))
cachedEitherUser = do
    eUserId <- cachedEitherUserId
    case eUserId of
        Left e -> return $ Left e 
        Right userId -> fmap eitherUser $ cached $ fmap UserCache $ getEitherUser userId

getEitherUser :: UserId -> Handler (Either String (Entity User))
getEitherUser userId = do
    result <- getUser userId
    return $ bimap (const "Token not connected to any user. Create user first.") id result

getEitherUserId :: Handler (Either String UserId)
getEitherUserId = do
    request <- waiRequest
    App {..} <- getYesod
    let authHeader = lookup "Authorization" $ W.requestHeaders request
    case authHeader of
        Nothing -> return $ Left "No Authorization header."
        Just token -> do
            result <- liftIO $ verifyUserToken appSettings $ fromStrict $ drop (length "Bearer ") token
            return $ bimap createErrorMessage (UserKey . pack) result

canCurrentUserAccessProject :: ProjectId -> Handler Bool
canCurrentUserAccessProject pId = do
    eProject <- runDB $ runExceptT $ getProject pId
    userId <- currentUserId
    return $ case eProject of
        Left _ -> False
        Right project -> userId `elem` projectContributorIds (entityVal project)

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
