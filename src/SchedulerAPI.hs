{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module SchedulerAPI(
	module Types,
	module MonadLog,
	module History,
	SchedulerMonad(..),
	takeCallWhilePossible,
	takeNextCallWhilePossible,
	SchedulerImpl(..),
	defSchedImpl,
) where

import Types
import History
import MonadLog
import Control.Monad.State
import Data.Maybe


-- |to implement a scheduling alg, implement this datatype
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
		sched_showSchedData ::
			schedData -> Maybe String
	}

class (MonadLog m, MonadState d m) => SchedulerMonad d m | m -> d where
	setTimer :: Time -> CallerInfo -> m ()
	takeCall :: CallerInfo -> m Bool
	takeNextCall :: m (Maybe CallerInfo)


{- |try to accept all calls in the list.
 - |return every caller for which this succeeded
-}
takeCallWhilePossible ::
	SchedulerMonad d m =>
	[CallerInfo] -> m [CallerInfo]
takeCallWhilePossible list =
	fmap catMaybes $ 
	forM list $ \caller ->
		do
			callTaken <- takeCall caller
			case callTaken of
				True -> return $ Just caller
				False -> return $ Nothing

{- |try to accept all calls in the caller queue
 - |return every caller for which this succeeded
-}
takeNextCallWhilePossible ::
	SchedulerMonad d m =>
	m [CallerInfo]
takeNextCallWhilePossible =
	do
		mCallerInfo <- takeNextCall
		case mCallerInfo of
			Just callerInfo ->
				(callerInfo:) <$> takeNextCallWhilePossible
			Nothing -> return []

-- |ignore every event:
defSchedImpl :: SchedulerImpl schedData
defSchedImpl = SchedulerImpl{
	sched_onIncomingCall = \_ _ _ -> return [],
	sched_onHangupCall = \_ _ _ -> return [],
	sched_onTimerEvent = \_ _ _ -> return (),
	sched_showSchedData = \_ -> Nothing
}
