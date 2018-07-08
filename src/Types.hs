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
		available_agents :: Int,
		-- agents_number :: Int,
		current_calls :: S.Set CallerInfo
	}
	deriving( Show, Read, Eq, Ord)

type TimeQ a = M.Map Time a


--------------------------------------------------
-- manipulate "SimulationState"
--------------------------------------------------

takeCall :: SimulationState -> Maybe SimulationState
takeCall simState@SimulationState{..} =
	if (available_agents == 0)
	then Nothing
	else
		do
			nextCaller <- listToMaybe simState_callerQ
			return $ simState{
				simState_callerQ = drop 1 simState_callerQ,
				available_agents = available_agents - 1,
				current_calls = S.insert nextCaller current_calls
			}

addCallerToQ :: CallerInfo -> SimulationState -> SimulationState
addCallerToQ callerInfo simState@SimulationState{..} =
	simState{ simState_callerQ = simState_callerQ ++ [callerInfo] }
