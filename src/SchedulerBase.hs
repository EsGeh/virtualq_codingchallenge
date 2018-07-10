{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module SchedulerBase where

import SchedulerAPI

import qualified Data.Set as S
import Control.Monad.State
import Data.Maybe
import Data.List


-- invariant: number of agents = simState_availableAgents + S.size simState_currentCalls
data Data
	= Data {
		simState_callerQ :: CallerQ,
		simState_availableAgents :: Int,
		simState_currentCalls :: S.Set CallerInfo
	}
	deriving( Show, Read, Eq, Ord)

-- |Queue of callers
type CallerQ = [CallerInfo]

--------------------------------------------------
-- manipulate "Data"
--------------------------------------------------

getCallFromQ :: Data -> Maybe CallerInfo
getCallFromQ simState@Data{..} =
	listToMaybe simState_callerQ

serveCallsWhilePossible ::
	MonadState Data m =>
	m [CallerInfo]
serveCallsWhilePossible =
	do
		mCallerInfo <- state serveNextCall
		case mCallerInfo of
			Nothing -> return []
			Just callerInfo ->
				(callerInfo:) <$> serveCallsWhilePossible

serveNextCall :: Data -> (Maybe CallerInfo, Data)
serveNextCall simState@Data{..} =
	case
		(do
			nextCaller <- listToMaybe simState_callerQ
			(nextCaller,) <$> serveCall nextCaller simState
		)
	of
		Nothing -> (Nothing, simState)
		Just (nextCaller, newData) -> (Just nextCaller, newData)

serveCall :: CallerInfo -> Data -> Maybe Data
serveCall callerInfo simState@Data{..} =
	if
		simState_availableAgents == 0
		|| callerInfo `notElem` simState_callerQ
	then Nothing
	else
			return $ simState{
				simState_callerQ = delete callerInfo $ simState_callerQ,
				simState_availableAgents = simState_availableAgents - 1,
				simState_currentCalls = S.insert callerInfo simState_currentCalls
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
