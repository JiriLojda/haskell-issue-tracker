module SharedTypes where

import Control.Monad.Trans.Maybe

import Import

data ErrorReason = NoProject | NoIssue

type ServiceReturn a = Handler (Either ErrorReason a)
type DBAction a = MaybeT (YesodDB App) a
