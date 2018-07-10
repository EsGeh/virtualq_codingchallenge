{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module SimulationState where

import Types


import qualified Data.Set as S
import Control.Monad.State
import Data.Maybe
import Data.List

-- invariant: number of agents = simState_availableAgents + S.size simState_currentCalls
data SimulationState
	= SimulationState {
		simState_callerQ :: CallerQ,
		simState_availableAgents :: Int,
		simState_currentCalls :: S.Set CallerInfo
	}
	deriving( Show, Read, Eq, Ord)

-- |Queue of callers
type CallerQ = [CallerInfo]

--------------------------------------------------
-- manipulate "SimulationState"
--------------------------------------------------

getCallFromQ :: SimulationState -> Maybe CallerInfo
getCallFromQ simState@SimulationState{..} =
	listToMaybe simState_callerQ

serveNextCall :: SimulationState -> (Maybe CallerInfo, SimulationState)
serveNextCall simState@SimulationState{..} =
	case
		(do
			nextCaller <- listToMaybe simState_callerQ
			(nextCaller,) <$> serveCall nextCaller simState
		)
	of
		Nothing -> (Nothing, simState)
		Just (nextCaller, newSimulationState) -> (Just nextCaller, newSimulationState)

serveCall :: CallerInfo -> SimulationState -> Maybe SimulationState
serveCall callerInfo simState@SimulationState{..} =
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

addCallerToQ :: CallerInfo -> SimulationState -> SimulationState
addCallerToQ callerInfo simState@SimulationState{..} =
	simState{ simState_callerQ = simState_callerQ ++ [callerInfo] }

hangupCall :: CallerInfo -> SimulationState -> Maybe SimulationState
hangupCall callerInfo simState@SimulationState{..} =
	if callerInfo `elem` simState_currentCalls
	then
		Just $ simState{
			simState_availableAgents = simState_availableAgents + 1,
			simState_currentCalls = S.delete callerInfo simState_currentCalls
		}
	else
		Nothing
