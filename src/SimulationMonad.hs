{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SimulationMonad where

import Types
import History
import SimulationState( SimulationState(..) )
import qualified SimulationState as SimState
import SchedulerAPI

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.Reader

data SimulationSettings
	= SimulationSettings {
		settings_density :: Float, -- ^ number of calls per hour
		settings_callDurationRange :: (Time, Time) -- ^ range of call durations
	}


-- |global simulation state
data GlobalState schedData
	= GlobalState {
		glob_god :: GodState,
		glob_simState :: SimulationState,
		glob_schedData :: schedData,
		glob_history :: History
}

-- |secret simulation state (things only god knows)
data GodState
	= GodState {
		godState_upcomingEvents :: TimeQ Event, -- ^ upcoming events
		godState_counter :: Int -- ^ internal counter
	}
	deriving( Show, Read, Eq, Ord)

godState_addEvent :: Time -> Event -> GodState -> GodState
godState_addEvent time event = godState_addEvents (M.singleton time event)

godState_addEvents :: TimeQ Event -> GodState -> GodState
godState_addEvents events s@GodState{..} =
	s{ 
		godState_upcomingEvents =
			godState_upcomingEvents `M.union` events
	}

--------------------------------------------------
-- pseudo lenses for `GlobalState` (boilerplate)
--------------------------------------------------

glob_mapToGod :: (GodState -> GodState) -> GlobalState schedData -> GlobalState schedData
glob_mapToGod f = runIdentity . glob_mapToGodM (return . f)

glob_mapToGodM :: Monad m => (GodState -> m GodState) -> (GlobalState schedData) -> m (GlobalState schedData)
glob_mapToGodM f x =
	do
		val <- f $ glob_god x
		return $ x{ glob_god = val }

glob_mapToSchedData :: (schedData -> schedData) -> GlobalState schedData -> GlobalState schedData
glob_mapToSchedData f = runIdentity . glob_mapToSchedDataM (return . f)

glob_mapToSchedDataM :: Monad m => (schedData -> m schedData) -> (GlobalState schedData) -> m (GlobalState schedData)
glob_mapToSchedDataM f x =
	do
		val <- f $ glob_schedData x
		return $ x{ glob_schedData = val }

glob_mapToSimState :: (SimulationState -> SimulationState) -> GlobalState schedData -> GlobalState schedData
glob_mapToSimState f = runIdentity . glob_mapToSimStateM (return . f)

glob_mapToSimStateM :: Monad m => (SimulationState -> m SimulationState) -> GlobalState schedData -> m (GlobalState schedData)
glob_mapToSimStateM f x =
	do
		val <- f $ glob_simState x
		return $ x{ glob_simState = val }

glob_mapToHistory :: (History -> History) -> GlobalState schedData -> GlobalState schedData
glob_mapToHistory f = runIdentity . glob_mapToHistoryM (return . f)

glob_mapToHistoryM :: Monad m => (History -> m History) -> GlobalState schedData -> m (GlobalState schedData)
glob_mapToHistoryM f x =
	do
		val <- f $ glob_history x
		return $ x{ glob_history = val }

godStateInit :: GodState
godStateInit = GodState {
	godState_upcomingEvents = M.empty,
	godState_counter = 0
}


--------------------------------------------------
-- api
--------------------------------------------------

getNextEvent ::
	Monad m =>
	StateT GodState m (Maybe (Time, Event))
getNextEvent =
	get >>= \GodState{..} ->
		return $ M.lookupMin godState_upcomingEvents

popNextEvent :: Monad m =>
	StateT GodState m (Maybe (Time, Event))
popNextEvent =
	do
		nextEvent <- getNextEvent
		modify $ \godState@GodState{..} ->
			godState{ godState_upcomingEvents = M.deleteMin godState_upcomingEvents }
		return nextEvent

--------------------------------------------------
-- state monad to store the main state of the simulation
--------------------------------------------------

type SimulationMonadT schedData m a = StateT (GlobalState schedData) m a

runSimulationMonad ::
	Monad m => 
	SimulationState -> schedData -> SimulationMonadT schedData m a -> m a
runSimulationMonad simStateInit schedDataInit =
	evalStateT `flip` initGlob
	where
		initGlob = GlobalState{
			glob_god = godStateInit,
			glob_simState = simStateInit,
			glob_schedData = schedDataInit,
			glob_history = M.empty
		}

-- history:
addToHistory :: Monad m => CallerInfo -> HistoryEntry -> SimulationMonadT schedData m ()
addToHistory callerInfo entry =
	modify $ glob_mapToHistory $ addEntry callerInfo entry

getHistory :: Monad m => SimulationMonadT schedData m History
getHistory = gets glob_history

-- helper functions for the SimulationMonadT

withGodState :: Monad m => StateT GodState m a -> SimulationMonadT schedData m a 
withGodState f =
	get >>= \glob@GlobalState{ glob_god = x } ->
		do
			(ret, x') <- lift $ runStateT f x
			put glob{ glob_god = x' }
			return ret

withSchedulerData :: Monad m => SimulationSettings -> Time -> SchedulerMonadT schedData m a -> SimulationMonadT schedData m a 
withSchedulerData simulationSettings time =
	(runReaderT `flip` (simulationSettings,time)) . fromSchedulerMonadT

withSimState :: Monad m => StateT SimulationState m a -> SimulationMonadT schedData m a 
withSimState f =
	get >>= \glob@GlobalState{ glob_simState = x } ->
		do
			(ret, x') <- lift $ runStateT f x
			put glob{ glob_simState = x' }
			return ret


getGodState :: Monad m => SimulationMonadT schedData m GodState
getGodState = gets glob_god

getSchedData :: Monad m => SimulationMonadT schedData m schedData
getSchedData = gets glob_schedData

getSimState :: Monad m => SimulationMonadT schedData m SimulationState
getSimState = gets glob_simState

modifyGodState :: Monad m => (GodState -> GodState) -> SimulationMonadT schedData m ()
modifyGodState = modify . glob_mapToGod

modifySchedData :: Monad m => (schedData -> schedData) -> SimulationMonadT schedData m ()
modifySchedData = modify . glob_mapToSchedData

modifySimState :: Monad m => (SimulationState -> SimulationState) -> SimulationMonadT schedData m ()
modifySimState = modify . glob_mapToSimState

putSimState ::
	Monad m =>
	SimulationState -> SimulationMonadT schedData m ()
putSimState x = modifySimState $ const x

putSchedData ::
	Monad m =>
	schedData -> SimulationMonadT schedData m ()
putSchedData x = modifySchedData $ const x

putGodState ::
	Monad m =>
	GodState -> SimulationMonadT schedData m ()
putGodState x = modifyGodState $ const x


--------------------------------------------------
-- the monad for the scheduling algorithm (hide implementation details from scheduler)
--------------------------------------------------

-- this is the monad in which the scheduling algorithm runs:
newtype SchedulerMonadT d m a = SchedulerMonadT { fromSchedulerMonadT :: ReaderT (SimulationSettings,Time) (StateT (GlobalState d) m) a }
	deriving( {-MonadTrans,-} Applicative, Monad, Functor )

instance (Monad m) => MonadState d (SchedulerMonadT d m) where
	get = SchedulerMonadT $ lift $ getSchedData
	put = SchedulerMonadT . lift . putSchedData

instance (MonadLog m, MonadRandom m) => SchedulerMonad d (SchedulerMonadT d m) where
	setTimer time callerInfo =
		SchedulerMonadT $ ReaderT $ \_ -> withGodState $ modify $
		godState_addEvent time (TimerEvent callerInfo)

	takeCall callerInfo =
		SchedulerMonadT $
		ReaderT $ \(SimulationSettings{..},time) ->
		getSimState >>= \simState -> 
			case SimState.serveCall callerInfo simState of
				Nothing -> return False
				Just newState ->
					do
						putSimState newState
						-- add 
						callDuration <- getRandomR settings_callDurationRange
						modifyGodState $ godState_addEvent (time+callDuration) $
							HangupCall callerInfo
						return True

	takeNextCall =
		SchedulerMonadT $
		ReaderT $ \(SimulationSettings{..},_) ->
		do
			mCallerInfo <- withSimState $ state SimState.serveNextCall
			case mCallerInfo of
				Just callerInfo ->
					do
						callDuration <- getRandomR settings_callDurationRange
						modifyGodState $ godState_addEvent callDuration $
							HangupCall callerInfo
						return $ Just callerInfo
				Nothing -> return Nothing

instance (MonadLog m) => MonadLog (SchedulerMonadT d m) where
	doLog str = SchedulerMonadT $ doLog str
