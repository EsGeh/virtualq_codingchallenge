{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module NaiveScheduler where

import SchedulerBase
import SchedulerAPI

import Control.Monad.State


type Data = ()

initData :: Data
initData = ()

impl :: SchedulerImpl Data
impl = defSchedImpl {
	sched_onIncomingCall = onIncomingCall,
	sched_onHangupCall = onHangupCall
}

onIncomingCall ::
	(SchedulerMonad Data m) =>
	Time -> History -> CallerInfo -> m [CallerInfo]
onIncomingCall _ _ _ =
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
onHangupCall _ _ _ =
	do
		servedCalls <- takeNextCallWhilePossible
		when (not $ null servedCalls) $
			schedLog $ concat [ "accepted calls: ", show servedCalls ]
		return servedCalls
