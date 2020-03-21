module ModelTypes where

-- import Yesod;
-- import Database.Persist.Sql
-- import Data.Text as Text
-- import Data.UUID as UUID

-- instance PersistField UUID.UUID where
--   toPersistValue = PersistDbSpecific . UUID.toASCIIBytes
--   fromPersistValue (PersistDbSpecific t) =
--     case UUID.fromASCIIBytes t of
--       Just x -> Right x
--       Nothing -> Left $ Text.pack $ "Invalid UUID"
--   fromPersistValue _ = Left $ Text.pack $ "Not PersistDBSpecific"

-- instance PersistFieldSql UUID.UUID where
--   sqlType _ = SqlString

-- instance PathPiece UUID.UUID where
--     toPathPiece = UUID.toText
--     fromPathPiece = UUID.fromText