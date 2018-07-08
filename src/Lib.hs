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
		density = 4,
		callDurationRange = (2 * minute, 20 * minute)
	}

minute = 1 / 60

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
			simulateOneHour (t+1)
			runMainLoop runtime (t+1)

simulateOneHour :: (MonadLog m) => Time -> SimulationMonadT m ()
simulateOneHour tMax =
	do
		mNextEvent <- withGodState getNextEvent
		case mNextEvent of
			Nothing ->
				do
					doLog $ concat [ "no next event!" ]
					return ()
			Just (t, event) ->
				if t >= tMax
				then
					do
						doLog $ concat [ "tMax reached!" ]
						return ()
				else
					do
						_ <- popNextEvent
						doLog $ concat [ "t = ", show t]
						case event of
							IncomingCall callerInfo ->
								do
									doLog $ concat [ "\tincoming call: ", show callerInfo]
									modifySimState $ addCallerToQ callerInfo
									withSimState serveCalls
							HangupCall callerInfo ->
								do
									doLog $ concat [ "\tcall ended: ", show callerInfo]
									modifySimState $ hangupCall callerInfo
									withSimState serveCalls
						simulateOneHour tMax
	

-- |while there are any free agents, take call:
serveCalls :: (MonadLog m, MonadState SimulationState m) => m ()
serveCalls =
	get >>= \simState@SimulationState{..} ->
	do
		case getCallFromQ simState of
			Nothing -> return ()
			Just nextCaller ->
				do
					let mNewState = takeCall simState
					case mNewState of
						Nothing -> return ()
						Just newState ->
							do
								doLog $ concat [ "\tcall ", show nextCaller, " accepted"]
								put newState
								serveCalls

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
						godState_upcomingEvents =
							(godState_upcomingEvents `M.union`) $
							(M.fromList $ callTimes `zip` (IncomingCall <$> callerIds))
							`M.union`
							(M.fromList $ zipWith (+) callTimes callDurations `zip` (HangupCall <$> callerIds)),
						godState_counter = godState_counter + number
					}
