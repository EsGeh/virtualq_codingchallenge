{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Lib(
	runSimulation
) where

import Types
import SimulationState
import SimulationMonad
import SchedulerAPI
import qualified Analysis
import qualified NaiveScheduler as NaiveSched
import qualified VQScheduler as VQSched

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Random
import Control.Monad.State
import Data.List
import Data.Maybe



data SimulationArgs
	= SimulationArgs {
		simArgs_simulationSettings :: SimulationSettings,
		simArgs_runtime :: Time -- ^ how many hours to simulate
	}

simulationArgs :: SimulationArgs
simulationArgs = SimulationArgs{
	simArgs_simulationSettings = SimulationSettings {
		settings_density = 10, -- ^ how many calls will arrive per hour
		settings_callDurationRange = (2 * minute, 20 * minute) -- ^ min and max duration of a call
	},
	simArgs_runtime = 3 -- simulate 3 hours
}

simStateInit :: SimulationState
simStateInit = SimulationState [] 2 S.empty

runSimulation :: IO ()
runSimulation =
	do
		doLog $ "----------------------------------------------"
		doLog $ "simulating with NaiveScheduler..."
		_ <- runSimulationMonad simStateInit NaiveSched.initData (runMainLoop (simArgs_simulationSettings simulationArgs) NaiveSched.impl (simArgs_runtime simulationArgs) 0)
		doLog $ "----------------------------------------------"
		doLog $ "simulating with VQScheduler..."
		_ <- runSimulationMonad simStateInit VQSched.initData (runMainLoop (simArgs_simulationSettings simulationArgs) VQSched.impl (simArgs_runtime simulationArgs) 0)
		return ()

runMainLoop ::
	(MonadLog m, MonadRandom m) =>
	SimulationSettings -> SchedulerImpl schedData ->
	Time -> Time -> SimulationMonadT (GlobalState schedData) m ()
runMainLoop simulationSettings schedImpl runtime t =
	if t >= runtime then return ()
	else
		do
			-- calculate distribution of incoming calls:
			withGodState $ spawnCallers simulationSettings t (t+1)
			-- run simulation:
			simulateOneHour simulationSettings schedImpl (t+1)
			runMainLoop simulationSettings schedImpl runtime (t+1)

simulateOneHour ::
	(MonadLog m, MonadRandom m) =>
	SimulationSettings -> SchedulerImpl schedData ->
	Time -> SimulationMonadT (GlobalState schedData) m ()
simulateOneHour simulationSettings schedImpl@SchedulerImpl{..} tMax =
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
						showSimulationState =<< getSimState
						showStatistics t =<< getHistory
						logSchedData sched_showSchedData =<< getSchedData
						doLog $ concat ["\tEvent: ", show event]
						case event of
							IncomingCall callerInfo ->
								do
									modifySimState $ addCallerToQ callerInfo
									addToHistory callerInfo $ HistoryEntry t IncomingCallEvent
									-- call scheduler:
									history <- getHistory
									acceptedCalls <- withSchedulerData simulationSettings t $
										sched_onIncomingCall t history callerInfo
									mapM_ (\acceptedCaller -> addToHistory acceptedCaller $ HistoryEntry t ServeCallEvent) acceptedCalls
							HangupCall callerInfo ->
								do
									modifySimState $ fmap (fromMaybe $ error "inconsistent state in HangupCall event!") $ hangupCall callerInfo
									addToHistory callerInfo $ HistoryEntry t HangupCallEvent
									-- call scheduler:
									history <- getHistory
									acceptedCalls <- withSchedulerData simulationSettings t $
										sched_onHangupCall t history callerInfo
									mapM_ (\acceptedCaller -> addToHistory acceptedCaller $ HistoryEntry t ServeCallEvent) acceptedCalls
							TimerEvent callerInfo ->
								do
									-- call scheduler:
									history <- getHistory
									withSchedulerData simulationSettings t $
										sched_onTimerEvent t history callerInfo
						-- print analysis data:
						simulateOneHour simulationSettings schedImpl tMax

showSimulationState :: MonadLog m => SimulationState -> m ()
showSimulationState SimulationState{..} =
	do
		doLog "\tCallcenter state:"
		doLog $
			unlines $ map concat $
			[ [ "\t\tqueue: ", show simState_callerQ ]
			, [ "\t\tavailable agents: ", show simState_availableAgents ]
			, [ "\t\tcurrent calls: ", show simState_currentCalls ]
			]

showStatistics :: MonadLog m => Time -> History -> m ()
showStatistics t history =
	do
		doLog "\tStatistics:"
		doLog $ unlines $ map concat $
			[ [ "\t\taverage waiting time: ", show $ Analysis.calcAvgWaitingTime t history ]
			, [ "\t\tmax waiting time: ", show $ Analysis.calcLongestWaitingTime t history ]
			, [ "\t\taverage serve time: ", show $ Analysis.calcAvgServeTime t history ]
			]

logSchedData :: MonadLog m => (schedData -> Maybe String) -> schedData -> m ()
logSchedData showSchedData schedData =
	case showSchedData schedData of
		Nothing -> return ()
		Just repr -> 
			do
				doLog $ "\tScheduler Infos:"
				doLog $ unlines $ map ("\t\t"++) $ lines $ repr

--------------------------------------------------
-- helper functions:
--------------------------------------------------

spawnCallers :: forall m . (MonadRandom m, MonadState GodState m) => SimulationSettings -> Time -> Time -> m ()
spawnCallers SimulationSettings{..} start end =
	let
		number = ceiling $ (end-start) * settings_density
	in
		do
			callTimes <-
				sort <$>
				take number <$>
				getRandomRs (start, end)
					:: m [Time]
			get >>= \godState@GodState{..} ->
				do
					let callerIds = CallerInfo <$> take number [godState_counter..]
					put $ godState{
						godState_upcomingEvents =
							(godState_upcomingEvents `M.union`) $
							(M.fromList $ callTimes `zip` (IncomingCall <$> callerIds)),
						godState_counter = godState_counter + number
					}
