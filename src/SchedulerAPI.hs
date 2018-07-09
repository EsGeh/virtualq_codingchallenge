{-# LANGUAGE FunctionalDependencies #-}
module SchedulerAPI(
	module Types,
	module MonadLog,
	SchedulerMonad(..),
) where

import Types
import MonadLog
import Control.Monad.State

class (MonadLog m, MonadState d m) => SchedulerMonad d m | m -> d where
	-- setTimer :: Time -> m ()
