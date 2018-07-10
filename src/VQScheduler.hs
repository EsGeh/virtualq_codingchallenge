{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module VQScheduler where

import SchedulerBase hiding( Data )
import SchedulerAPI
import qualified Analysis
import qualified Utils

import qualified Data.Set as S
import Control.Monad.State
import Data.Maybe


-- | the virtualQ:
type Data = [CallerInfo]

initData = []

impl = defSchedImpl {
	sched_onIncomingCall = onIncomingCall,
	sched_onHangupCall = onHangupCall,
	sched_onTimerEvent = onTimerEvent,
	sched_showSchedData = showSchedData
}

showSchedData = Just . show

onIncomingCall ::
	(SchedulerMonad Data m) =>
	Time -> History -> CallerInfo -> m [CallerInfo]
onIncomingCall t history callerInfo =
	do
		acceptedCalls <- takeCallsWhilePossible
		if null acceptedCalls
			then
				do
					schedLog $ concat [ "all agents are busy!" ]
					setTimer (t+countdownTime) callerInfo
					-- modify $ (callerInfo:)
			else
				schedLog $ concat [ "served calls: ", show acceptedCalls ]
		return acceptedCalls
	where
		countdownTime =
			max currentWaitingTime $
			longestWaitingTime + avgServeTime
		currentWaitingTime = Analysis.calcAvgWaitingTime t history
		longestWaitingTime = Analysis.calcLongestWaitingTime t history
		avgServeTime = Analysis.calcAvgServeTime t history

onHangupCall ::
	(SchedulerMonad Data m) =>
	Time -> History -> CallerInfo -> m [CallerInfo]
onHangupCall _ _ callerInfo =
	return []
	-- serveCallsWhilePossible

onTimerEvent ::
	(SchedulerMonad Data m) =>
	Time -> History -> CallerInfo -> m ()
onTimerEvent _ _ callerInfo =
	do
		serveCallAndReport callerInfo
		-- Utils.modifyMaybe $ serveCall callerInfo

serveCallAndReport ::
	(SchedulerMonad Data m) =>
	CallerInfo -> m ()
serveCallAndReport callerInfo =
	do
		callTaken <- takeCall callerInfo
		case callTaken of
			False ->
				schedLog $ concat ["WARNING: couldn't accept call!", show callerInfo]
			True ->
				schedLog $ concat ["accepted call ", show callerInfo]
