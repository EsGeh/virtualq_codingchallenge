{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module VQScheduler where

import SchedulerBase
import SchedulerAPI
import qualified Analysis

import Control.Monad.State
import Data.List
import Control.Monad.Identity


-- | the virtualQ:
data Data =
	Data{
		data_virtualQ :: [CallerInfo], -- ^ customers waiting to be called back
		data_urgentQ :: [CallerInfo ] -- ^ customers to call back immediately 
	}

data_mapToVirtualQ ::
	([CallerInfo] -> [CallerInfo]) -> Data -> Data
data_mapToVirtualQ f = runIdentity . data_mapToVirtualQM (return . f)

data_mapToVirtualQM ::
	Monad m =>
	([CallerInfo] -> m [CallerInfo]) -> Data -> m Data
data_mapToVirtualQM f x@Data{..} =
	do
		val <- f $ data_virtualQ
		return $ x{ data_virtualQ = val }

data_mapToUrgentList ::
	([CallerInfo] -> [CallerInfo]) -> Data -> Data
data_mapToUrgentList f = runIdentity . data_mapToUrgentListM (return . f)

data_mapToUrgentListM ::
	Monad m =>
	([CallerInfo] -> m [CallerInfo]) -> Data -> m Data
data_mapToUrgentListM f x@Data{..} =
	do
		val <- f $ data_urgentQ
		return $ x{ data_urgentQ = val }

initData :: Data
initData = Data [] []

impl :: SchedulerImpl Data
impl = defSchedImpl {
	sched_onIncomingCall = onIncomingCall,
	sched_onHangupCall = onHangupCall,
	sched_onTimerEvent = onTimerEvent,
	sched_showSchedData = showSchedData
}

showSchedData :: Data -> Maybe String
showSchedData Data{..} = Just $ unlines $ map concat $
	[ [ "virtualQ: ", show data_virtualQ ]
	, [ "urgentQ: ", show data_urgentQ ]
	]

onIncomingCall ::
	(SchedulerMonad Data m) =>
	Time -> History -> CallerInfo -> m [CallerInfo]
onIncomingCall t history callerInfo =
	do
		--acceptedCalls <- takeCallsWhilePossible
		callTaken <- takeCall callerInfo
		case callTaken of
			True ->
				do
					schedLog $ concat [ "served call: ", show callerInfo ]
					return [callerInfo]
			False ->
				do
					schedLog $ concat [ "all agents are busy! (setting timer to t+", show countdownTime, ")" ]
					setTimer (t+countdownTime) callerInfo
					-- add caller to the virtual Q:
					modify $ data_mapToVirtualQ (++[callerInfo])
					return []
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
onHangupCall _ _ _ =
	do
		-- try to call back urgent customers:
		urgentQ <- data_urgentQ <$> get
		calledBack <- takeCallWhilePossible urgentQ
		schedLog $ concat [ "called back: ", show calledBack ]
		modify $ data_mapToUrgentList $ (\\calledBack)
		-- if there are still capacities, accept calls:
		acceptedCalls <- takeNextCallWhilePossible
		return $ calledBack ++ acceptedCalls

onTimerEvent ::
	(SchedulerMonad Data m) =>
	Time -> History -> CallerInfo -> m ()
onTimerEvent _ _ callerInfo =
	do
		-- copy caller from virtualQ -> urgentQ
		schedLog $ concat [ show callerInfo, " becomes urgent..." ]
		modify $ data_mapToVirtualQ $ delete callerInfo
		modify $ data_mapToUrgentList $ (++[callerInfo])
