module SharedTypes where

import Control.Monad.Trans.Except
import Yesod.Persist.Core (YesodDB)
import Yesod.Core.Handler (HandlerFor)

import AppModel (App)

data ErrorReason = NoProject | ArchivedProject | NoIssue | NoIssueComment | NoWorkflowStep | NoUser

type ServiceReturn a = HandlerFor App (Either ErrorReason a)
type DBAction a = ExceptT ErrorReason (YesodDB App) a
