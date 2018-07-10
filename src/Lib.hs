{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Lib(
	runSimulation
) where

import Types
import SimulationMonad
import SchedulerAPI
import History
import qualified Analysis
import MonadLog
import qualified NaiveScheduler as NaiveSched
import qualified VQScheduler as VQSched

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
		doLog $ "----------------------------------------------"
		doLog $ "simulating with NaiveScheduler..."
		_ <- runSimulationMonad NaiveSched.initData (runMainLoop NaiveSched.impl 3 0)
		doLog $ "----------------------------------------------"
		doLog $ "simulating with VQScheduler..."
		_ <- runSimulationMonad VQSched.initData (runMainLoop VQSched.impl 3 0)
		return ()

runMainLoop ::
	(MonadLog m, MonadRandom m) =>
	SchedulerImpl schedData ->
	Time -> Time -> SimulationMonadT (GlobalState schedData) m ()
runMainLoop schedImpl runtime t =
	if t >= runtime then return ()
	else
		do
			-- calculate distribution of incoming calls:
			withGodState $ spawnCallers callersArgs t (t+1)
			-- run simulation:
			simulateOneHour schedImpl (t+1)
			runMainLoop schedImpl runtime (t+1)

simulateOneHour ::
	(MonadLog m) =>
	SchedulerImpl schedData ->
	Time -> SimulationMonadT (GlobalState schedData) m ()
simulateOneHour schedImpl@SchedulerImpl{..} tMax =
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
						doLog $ concat [ "t = ", show t, ": ", show event]
						case event of
							IncomingCall callerInfo ->
								do
									addToHistory callerInfo $ HistoryEntry t IncomingCallEvent
									history <- getHistory
									-- call scheduler:
									acceptedCalls <- withSimState $
										sched_onIncomingCall t history callerInfo
									mapM_ (\callerInfo -> addToHistory callerInfo $ HistoryEntry t ServeCallEvent) acceptedCalls
							HangupCall callerInfo ->
								do
									addToHistory callerInfo $ HistoryEntry t HangupCallEvent
									-- call scheduler:
									history <- getHistory
									acceptedCalls <- withSimState $
										sched_onHangupCall t history callerInfo
									mapM_ (\callerInfo -> addToHistory callerInfo $ HistoryEntry t ServeCallEvent) acceptedCalls
							TimerEvent callerInfo ->
								do
									-- call scheduler:
									history <- getHistory
									withSimState $
										sched_onTimerEvent t history callerInfo
						-- print analysis data:
						showStatistics t =<< getHistory
						logSchedState sched_showSchedState =<< getSimState
						simulateOneHour schedImpl tMax

showStatistics t history =
	do
		doLog "\tStatistics:"
		doLog $ unlines $ map concat $
			[ [ "\t\taverage waiting time: ", show $ Analysis.calcAvgWaitingTime t history ]
			, [ "\t\tmax waiting time: ", show $ Analysis.calcLongestWaitingTime t history ]
			, [ "\t\taverage serve time: ", show $ Analysis.calcAvgServeTime t history ]
			]
logSchedState showSchedState schedState =
	do
		doLog $ "\tScheduler Infos:"
		doLog $ unlines $ map ("\t\t"++) $ lines $
			showSchedState schedState

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
