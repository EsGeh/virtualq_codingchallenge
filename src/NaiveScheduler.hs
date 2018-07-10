{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module NaiveScheduler where

import SchedulerBase
import SchedulerAPI

import qualified Data.Set as S
import Control.Monad.State
import Data.Maybe


initData = Data [] 2 S.empty

impl = defSchedImpl {
	sched_onIncomingCall = onIncomingCall,
	sched_onHangupCall = onHangupCall,
	sched_showSchedState = showSchedState
}

onIncomingCall ::
	(SchedulerMonad Data m) =>
	Time -> History -> CallerInfo -> m [CallerInfo]
onIncomingCall _ _ callerInfo =
	do
		modify $ addCallerToQ callerInfo
		acceptedCalls <- serveCallsWhilePossible
		if null acceptedCalls
			then
				schedLog $ concat [ "all agents are busy!" ]
			else
				schedLog $ concat [ "served calls: ", show acceptedCalls ]
		return acceptedCalls

onHangupCall ::
	(SchedulerMonad Data m) =>
	Time -> History -> CallerInfo -> m [CallerInfo]
onHangupCall _ _ callerInfo =
	do
		modify $ hangupCall callerInfo
		servedCalls <- serveCallsWhilePossible
		when (not $ null servedCalls) $
			schedLog $ concat [ "served calls: ", show servedCalls ]
		return servedCalls
