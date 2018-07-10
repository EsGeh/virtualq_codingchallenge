{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module NaiveScheduler where

import SchedulerBase
import SchedulerAPI

import qualified Data.Set as S
import Control.Monad.State
import Data.Maybe


type Data = ()

initData = ()

impl = defSchedImpl {
	sched_onIncomingCall = onIncomingCall,
	sched_onHangupCall = onHangupCall
}

onIncomingCall ::
	(SchedulerMonad Data m) =>
	Time -> History -> CallerInfo -> m [CallerInfo]
onIncomingCall _ _ callerInfo =
	do
		acceptedCalls <- takeCallsWhilePossible
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
		servedCalls <- takeCallsWhilePossible
		when (not $ null servedCalls) $
			schedLog $ concat [ "served calls: ", show servedCalls ]
		return servedCalls
