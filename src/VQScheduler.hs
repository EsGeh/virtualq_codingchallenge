{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module VQScheduler where

import SchedulerBase
import SchedulerAPI
import qualified Analysis
import qualified Utils

import qualified Data.Set as S
import Control.Monad.State
import Data.Maybe


initData = Data [] 2 S.empty

impl = defSchedImpl {
	sched_onIncomingCall = onIncomingCall,
	sched_onHangupCall = onHangupCall,
	sched_onTimerEvent = onTimerEvent,
	sched_showSchedState = showSchedState
}

onIncomingCall ::
	(SchedulerMonad Data m) =>
	Time -> History -> CallerInfo -> m [CallerInfo]
onIncomingCall t history callerInfo =
	do
		modify $ addCallerToQ callerInfo
		acceptedCalls <- serveCallsWhilePossible
		if null acceptedCalls
			then
				do
					schedLog $ concat [ "all agents are busy!" ]
					setTimer (t+countdownTime) callerInfo
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
	do
		modify $ hangupCall callerInfo
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
		mNewState <- serveCall callerInfo <$> get
		case mNewState of
			Nothing ->
				schedLog $ concat ["WARNING: couldn't accept call!", show callerInfo]
			Just newState ->
				do
					schedLog $ concat ["accepted call ", show callerInfo]
