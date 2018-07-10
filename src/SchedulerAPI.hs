{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}
module SchedulerAPI(
	module Types,
	module MonadLog,
	module History,
	SchedulerMonad(..),
	SchedulerImpl(..),
	defSchedImpl,
) where

import Types
import History
import MonadLog
import Control.Monad.State


class (MonadLog m, MonadState d m) => SchedulerMonad d m | m -> d where
	setTimer :: Time -> CallerInfo -> m ()

data SchedulerImpl schedData
	= SchedulerImpl {
		sched_onIncomingCall ::
			forall m .
			(SchedulerMonad schedData m) =>
			Time -> History -> CallerInfo -> m [CallerInfo],
		sched_onHangupCall ::
			forall m .
			(SchedulerMonad schedData m) =>
			Time -> History -> CallerInfo -> m [CallerInfo],
		sched_onTimerEvent ::
			forall m .
			(SchedulerMonad schedData m) =>
			Time -> History -> CallerInfo -> m (),
		sched_showSchedState ::
			schedData -> String
	}

-- |ignore every event:
defSchedImpl :: SchedulerImpl schedData
defSchedImpl = SchedulerImpl{
	sched_onIncomingCall = \_ _ _ -> return [],
	sched_onHangupCall = \_ _ _ -> return [],
	sched_onTimerEvent = \_ _ _ -> return (),
	sched_showSchedState = \_ -> "sched_showSchedState not implemented!"
}
