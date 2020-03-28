module SharedTypes where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except

import Import

data ErrorReason = NoProject | ArchivedProject | NoIssue | NoIssueComment

type ServiceReturn a = Handler (Either ErrorReason a)
type DBAction a = ExceptT ErrorReason (YesodDB App) a