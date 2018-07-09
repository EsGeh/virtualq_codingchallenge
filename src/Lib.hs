{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Lib(
	runSimulation
) where

import Types
import SimulationMonad
import History
import Analysis
import MonadLog
import qualified Scheduler as Sched

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Writer
import System.IO
import Data.List
import Control.Monad.RWS


data CallersArgs
	= CallersArgs {
		density :: Float, -- ^ number of calls per hour
		callDurationRange :: (Time, Time) -- ^ range of call durations
	}

callersArgs =
	CallersArgs {
		density = 10,
		callDurationRange = (2 * minute, 20 * minute)
	}

minute = 1 / 60

runSimulation :: IO ()
runSimulation =
	do
		_ <-
			runSimulationMonad Sched.initData (runMainLoop 3 0)
		return ()

runMainLoop ::
	(MonadLog m, MonadRandom m) =>
	Time -> Time -> SimulationMonadT (GlobalState Sched.Data) m ()
runMainLoop runtime t =
	if t >= runtime then return ()
	else
		do
			-- calculate distribution of incoming calls:
			withGodState $ spawnCallers callersArgs t (t+1)
			-- run simulation:
			simulateOneHour (t+1)
			runMainLoop runtime (t+1)

simulateOneHour :: (MonadLog m) => Time -> SimulationMonadT (GlobalState Sched.Data) m ()
simulateOneHour tMax =
	do
		mNextEvent <- withGodState getNextEvent
		case mNextEvent of
			Nothing ->
				do
					-- doLog $ concat [ "no next event!" ]
					return ()
			Just (t, event) ->
				if t >= tMax
				then
					do
						-- doLog $ concat [ "tMax reached!" ]
						return ()
				else
					do
						_ <- withGodState popNextEvent
						doLog $ concat [ "t = ", show t]
						case event of
							IncomingCall callerInfo ->
								do
									doLog $ concat [ "\tincoming call: ", show callerInfo]
									addToHistory callerInfo $ HistoryEntry t IncomingCallEvent
									-- call scheduler:
									acceptedCalls <- withSimState $
										Sched.onIncomingCall callerInfo
									mapM_ (\callerInfo -> addToHistory callerInfo $ HistoryEntry t ServeCallEvent) acceptedCalls
							HangupCall callerInfo ->
								do
									addToHistory callerInfo $ HistoryEntry t HangupCallEvent
									doLog $ concat [ "\tcall ended: ", show callerInfo]
									-- call scheduler:
									acceptedCalls <- withSimState $
										Sched.onHangupCall callerInfo
									mapM_ (\callerInfo -> addToHistory callerInfo $ HistoryEntry t ServeCallEvent) acceptedCalls
						-- print analysis data:
						history <- getHistory
						doLog $ concat $ ["\tavg waiting time: ", show $ calcAvgWaitingTime history ]
						simulateOneHour tMax

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
