{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Scheduler where

import SchedulerAPI

import qualified Data.Set as S
import Control.Monad.State
import Data.Maybe


-- invariant: number of agents = simState_availableAgents + S.size simState_currentCalls
data Data
	= Data {
		simState_callerQ :: CallerQ,
		simState_availableAgents :: Int,
		simState_currentCalls :: S.Set CallerInfo
	}
	deriving( Show, Read, Eq, Ord)

initData = Data [] 2 S.empty

onIncomingCall ::
	(SchedulerMonad Data m) =>
	CallerInfo -> m [CallerInfo]
onIncomingCall callerInfo =
	--error "todo"
	do
		modify $ addCallerToQ callerInfo
		acceptedCalls <- serveCalls
		when (null acceptedCalls) $
			do
				doLog $ concat [ "\tall agents are busy!"]
				waitingQ <- simState_callerQ  <$> get
				doLog $ concat [ "\twaiting Q: ", show waitingQ]
		return acceptedCalls

onHangupCall ::
	(SchedulerMonad Data m) =>
	CallerInfo -> m [CallerInfo]
onHangupCall callerInfo =
	do
		modify $ hangupCall callerInfo
		serveCalls

--------------------------------------------------
-- manipulate "Data"
--------------------------------------------------

getCallFromQ :: Data -> Maybe CallerInfo
getCallFromQ simState@Data{..} =
	listToMaybe simState_callerQ

takeCall :: Data -> Maybe Data
takeCall simState@Data{..} =
	if (simState_availableAgents == 0)
	then Nothing
	else
		do
			nextCaller <- listToMaybe simState_callerQ
			return $ simState{
				simState_callerQ = drop 1 simState_callerQ,
				simState_availableAgents = simState_availableAgents - 1,
				simState_currentCalls = S.insert nextCaller simState_currentCalls
			}

addCallerToQ :: CallerInfo -> Data -> Data
addCallerToQ callerInfo simState@Data{..} =
	simState{ simState_callerQ = simState_callerQ ++ [callerInfo] }

hangupCall :: CallerInfo -> Data -> Data
hangupCall callerInfo simState@Data{..} =
	simState{
		simState_availableAgents = simState_availableAgents + 1,
		simState_currentCalls = S.delete callerInfo simState_currentCalls
	}

serveCalls ::
	(SchedulerMonad Data m) =>
	m [CallerInfo]
serveCalls =
	get >>= \simState@Data{..} ->
	do
		case getCallFromQ simState of
			Nothing -> return []
			Just nextCaller ->
				do
					let mNewState = takeCall simState
					case mNewState of
						Nothing -> return []
						Just newState ->
							do
								--doLog $ concat [ "\tcall ", show nextCaller, " accepted"]
								put newState
								furtherCalls <- serveCalls
								return $ nextCaller : furtherCalls
