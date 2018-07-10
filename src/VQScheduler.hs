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
	sched_onTimerEvent = onTimerEvent
}

onIncomingCall ::
	(SchedulerMonad Data m) =>
	Time -> History -> CallerInfo -> m [CallerInfo]
onIncomingCall t history callerInfo =
	do
		modify $ addCallerToQ callerInfo
		acceptedCalls <- serveCallsWhilePossible
		when (null acceptedCalls) $ -- if all agents are busy...
			do
				doLog $ concat [ "\tall agents are busy!"]
				setTimer (t+countdownTime) callerInfo
				waitingQ <- simState_callerQ  <$> get
				doLog $ concat [ "\twaiting Q: ", show waitingQ]
		return acceptedCalls
	where
		countdownTime =
			max currentWaitingTime $
			longestWaitingTime + avgServeTime
		currentWaitingTime = Analysis.calcAvgWaitingTime history
		longestWaitingTime = Analysis.calcLongestWaitingTime history
		avgServeTime = Analysis.calcAvgServeTime history

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
		doLog $ concat [ "\ttimer event!"]
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
				doLog $ concat ["WARNING: couldn't accept call!", show callerInfo]
			Just newState ->
				do
					doLog $ concat ["accepted call ", show callerInfo]
					put newState
