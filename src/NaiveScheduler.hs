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
	sched_onHangupCall = onHangupCall
}

onIncomingCall ::
	(SchedulerMonad Data m) =>
	Time -> History -> CallerInfo -> m [CallerInfo]
onIncomingCall _ _ callerInfo =
	do
		modify $ addCallerToQ callerInfo
		acceptedCalls <- serveCallsWhilePossible
		when (null acceptedCalls) $
			do
				doLog $ concat [ "\tall agents are busy!"]
				waitingQ <- simState_callerQ  <$> get
				doLog $ concat [ "\twaiting Q: ", show waitingQ]
		return acceptedCalls

onHangupCall ::
	(SchedulerMonad Data m) =>
	Time -> History -> CallerInfo -> m [CallerInfo]
onHangupCall _ _ callerInfo =
	do
		modify $ hangupCall callerInfo
		serveCallsWhilePossible
