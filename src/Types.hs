{-# LANGUAGE RecordWildCards #-}
module Types where

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe


-- |Queue of callers
type CallerQ = [CallerInfo]

-- |Information about a caller
data CallerInfo = CallerInfo CallerId
	deriving( Show, Read, Eq, Ord)

type CallerId = Int

-- |time in hours
type Time = Float

data SimulationState
	= SimulationState {
		simState_callerQ :: CallerQ,
		simState_availableAgents :: Int,
		-- agents_number :: Int,
		simState_currentCalls :: S.Set CallerInfo
	}
	deriving( Show, Read, Eq, Ord)

type TimeQ a = M.Map Time a


--------------------------------------------------
-- manipulate "SimulationState"
--------------------------------------------------

takeCall :: SimulationState -> Maybe SimulationState
takeCall simState@SimulationState{..} =
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

addCallerToQ :: CallerInfo -> SimulationState -> SimulationState
addCallerToQ callerInfo simState@SimulationState{..} =
	simState{ simState_callerQ = simState_callerQ ++ [callerInfo] }

hangupCall :: CallerInfo -> SimulationState -> SimulationState
hangupCall callerInfo simState@SimulationState{..} =
	simState{
		simState_availableAgents = simState_availableAgents + 1,
		simState_currentCalls = S.delete callerInfo simState_currentCalls
	}
