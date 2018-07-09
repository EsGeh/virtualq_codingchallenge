{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}
module SchedulerAPI(
	module Types,
	module MonadLog,
	SchedulerMonad(..),
	SchedulerImpl(..),
	defSchedImpl,
) where

import Types
import MonadLog
import Control.Monad.State


class (MonadLog m, MonadState d m) => SchedulerMonad d m | m -> d where
	setTimer :: Time -> CallerInfo -> m ()

data SchedulerImpl schedData
	= SchedulerImpl {
		sched_onIncomingCall ::
			forall m .
			(SchedulerMonad schedData m) =>
			CallerInfo -> m [CallerInfo],
		sched_onHangupCall ::
			forall m .
			(SchedulerMonad schedData m) =>
			CallerInfo -> m [CallerInfo],
		sched_onTimerEvent ::
			forall m .
			(SchedulerMonad schedData m) =>
			CallerInfo -> m ()
	}

-- |ignore every event:
defSchedImpl :: SchedulerImpl schedData
defSchedImpl = SchedulerImpl{
	sched_onIncomingCall = const $ return [],
	sched_onHangupCall = const $ return [],
	sched_onTimerEvent = const $ return ()
}
