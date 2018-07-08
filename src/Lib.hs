{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Lib(
	runSimulation
) where

import Types
import SimulationMonad
import MonadLog

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Random
import Control.Monad.State
import System.IO
import Data.List

data CallersArgs
	= CallersArgs {
		density :: Float,
		callDurationRange :: (Time, Time)
	}

callersArgs =
	CallersArgs {
		density = 10,
		callDurationRange = (2,20)
	}

runSimulation :: IO ()
runSimulation =
	do
		_ <- evalStateT (evalStateT (runMainLoop 3 0) godStateInit) initState
		return ()

initState = SimulationState [] 50 S.empty

runMainLoop ::
	(MonadLog m, MonadRandom m) =>
	Time -> Time -> SimulationMonadT m ()
runMainLoop runtime t =
	if t >= runtime then return ()
	else
		do
			-- calculate distribution of incoming calls:
			withGodState $ spawnCallers callersArgs t (t+1)
			-- run simulation:
			simulateOneHour t
			runMainLoop runtime (t+1)

simulateOneHour :: (MonadLog m) => Time -> SimulationMonadT m ()
simulateOneHour t =
	do
		doLog $ concat [ "simulateOneHour t = ", show t]
		withSimState $ get >>= \simState ->
			doLog $ concat [ "\tstate = ", show simState]
		withSimState serveCalls
		-- hangupCalls
		mNextCall <- withGodState popNextUpcomingCall
		case mNextCall of
			Nothing -> return ()
			Just (eventT, callerInfo) ->
				do
					modifySimState $ addCallerToQ callerInfo
					simulateOneHour eventT

{-
hangupCalls :: SimulationMonadT m ()
hangupCalls = undefined
-}

-- 
serveCalls :: MonadState SimulationState m => m ()
serveCalls =
	get >>= \simState ->
	case takeCall simState of
		Nothing -> return ()
		Just newSimState -> put newSimState >> serveCalls

--------------------------------------------------
-- helper functions:
--------------------------------------------------

spawnCallers :: forall m . (MonadRandom m, MonadState GodState m) => CallersArgs -> Time -> Time -> m ()
spawnCallers CallersArgs{..} start end =
	let
		number = ceiling $ (end-start)*density
	in
		do
			callTimes <-
				sort <$>
				take number <$>
				getRandomRs (start, end)
				:: m [Time]
			callDurations <-
				take number <$>
				getRandomRs callDurationRange
				:: m [Time]
			get >>= \godState@GodState{..} ->
				do
					let callerIds = CallerInfo <$> take number [godState_counter..]
					put $ godState{
						godState_upcomingCalls =
							M.fromList $ callTimes `zip` callerIds,
						godState_callEndTimes =
							M.fromList $ (zipWith (+) callTimes callDurations) `zip` callerIds,
						godState_counter = godState_counter + number
					}
