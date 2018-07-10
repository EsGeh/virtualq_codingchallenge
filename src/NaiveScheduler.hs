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
		acceptedCalls <- takeNextCallWhilePossible
		if null acceptedCalls
			then
				schedLog $ concat [ "all agents are busy!" ]
			else
				schedLog $ concat [ "accepted calls: ", show acceptedCalls ]
		return acceptedCalls

onHangupCall ::
	(SchedulerMonad Data m) =>
	Time -> History -> CallerInfo -> m [CallerInfo]
onHangupCall _ _ callerInfo =
	do
		servedCalls <- takeNextCallWhilePossible
		when (not $ null servedCalls) $
			schedLog $ concat [ "accepted calls: ", show servedCalls ]
		return servedCalls
