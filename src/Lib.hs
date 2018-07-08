{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Lib(
	runSimulation
) where

import Types
import MonadLog

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Random
import Control.Monad.State
import System.IO

runSimulation :: IO ()
runSimulation =
	do
		_ <- evalStateT (evalStateT (runMainLoop 3 0) godStateInit) initState
		return ()
	where
		initState = SimulationState [] 50 S.empty
		godStateInit = GodState {
			godState_upcomingCalls = M.empty,
			godState_counter = 0
		}


data GodState
	= GodState {
		godState_upcomingCalls :: TimeQ CallerInfo,
		godState_counter :: Int
	}

type SimulationMonadT m a = StateT GodState (StateT SimulationState m) a
 
withGodState :: Monad m => StateT GodState m a -> SimulationMonadT m a 
withGodState f =
	get >>= \g ->
		do
			(ret, g') <- lift $ lift $ runStateT f g
			put g'
			return ret

withSimState :: Monad m => StateT SimulationState m a -> SimulationMonadT m a 
withSimState f =
	lift f

getGodState :: Monad m => SimulationMonadT m GodState
getGodState = get
getSimState :: Monad m => SimulationMonadT m SimulationState
getSimState = withSimState $ get

modifySimState :: Monad m => (SimulationState -> SimulationState) -> SimulationMonadT m ()
modifySimState = lift . modify
modifyGodState :: Monad m => (GodState -> GodState) -> SimulationMonadT m ()
modifyGodState = modify

putSimState x = modifySimState $ const x
putGodState x = modifyGodState $ const x

runMainLoop ::
	(MonadLog m, MonadRandom m) =>
	Time -> Time -> SimulationMonadT m ()
runMainLoop runtime t =
	if t >= runtime then return ()
	else
		do
			-- calculate distribution of incoming calls:
			withGodState $ spawnCallers 10 t (t+1)
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
		getGodState >>= \godState@GodState{..} ->
			do
				let mNextCall = M.minViewWithKey godState_upcomingCalls
				case mNextCall of
					Nothing -> return ()
					Just ((eventT, callerInfo), futureCallsRest) ->
						do
							modifySimState $ addCallerToQ callerInfo
							putGodState $ godState{ godState_upcomingCalls = futureCallsRest }
							simulateOneHour eventT

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

spawnCallers :: forall m . (MonadRandom m, MonadState GodState m) => Float -> Time -> Time -> m ()
spawnCallers density start end =
	let
		number = ceiling $ (end-start)*density
	in
		do
			eventTimes <-
				take number <$> getRandomRs (start, end)
				:: m [Time]
			get >>= \godState@GodState{..} ->
				do
					let callerIds = CallerInfo <$> take number [godState_counter..]
					put $ godState{
						godState_upcomingCalls = M.fromList $ eventTimes `zip` callerIds,
						godState_counter = godState_counter + number
					}
