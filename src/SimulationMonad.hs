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
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Monad.Identity


-- global simulation state
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

glob_mapToGod f = runIdentity . glob_mapToGodM (return . f)
glob_mapToGodM :: Monad m => (GodState -> m GodState) -> (GlobalState schedData) -> m (GlobalState schedData)
glob_mapToGodM f x =
	do
		val <- f $ glob_god x
		return $ x{ glob_god = val }

glob_mapToSchedData f = runIdentity . glob_mapToSchedDataM (return . f)
glob_mapToSchedDataM :: Monad m => (schedData -> m schedData) -> (GlobalState schedData) -> m (GlobalState schedData)
glob_mapToSchedDataM f x =
	do
		val <- f $ glob_schedData x
		return $ x{ glob_schedData = val }

glob_mapToSimState f = runIdentity . glob_mapToSimStateM (return . f)

glob_mapToSimStateM :: Monad m => (SimulationState -> m SimulationState) -> GlobalState schedData -> m (GlobalState schedData)
glob_mapToSimStateM f x =
	do
		val <- f $ glob_simState x
		return $ x{ glob_simState = val }

glob_mapToHistory f = runIdentity . glob_mapToHistoryM (return . f)

glob_mapToHistoryM :: Monad m => (History -> m History) -> GlobalState schedData -> m (GlobalState schedData)
glob_mapToHistoryM f x =
	do
		val <- f $ glob_history x
		return $ x{ glob_history = val }

godStateInit = GodState {
	godState_upcomingEvents = M.empty,
	godState_counter = 0
}


--------------------------------------------------
-- api
--------------------------------------------------

getNextEvent :: Monad m =>
	StateT GodState m (Maybe (Time, Event))
getNextEvent =
	get >>= \godState@GodState{..} ->
		return $ M.lookupMin godState_upcomingEvents

popNextEvent :: Monad m =>
	StateT GodState m (Maybe (Time, Event))
popNextEvent =
	do
		next <- getNextEvent
		modify $ \godState@GodState{..} ->
			godState{ godState_upcomingEvents = M.deleteMin godState_upcomingEvents }
		return next

--------------------------------------------------
-- state monad to store the main state
--------------------------------------------------

type SimulationMonadT schedData m a = StateT schedData m a
-- (read, write, state)

runSimulationMonad ::
	Monad m => 
	SimulationState -> schedData -> SimulationMonadT (GlobalState schedData) m a -> m a
runSimulationMonad simStateInit schedDataInit =
	evalStateT `flip` init
	where
		init = GlobalState{
			glob_god = godStateInit,
			glob_simState = simStateInit,
			glob_schedData = schedDataInit,
			glob_history = M.empty
		}

-- this is the monad in which the scheduler runs:
newtype SchedulerMonadT d m a = SchedulerMonadT { fromSchedulerMonadT :: StateT (GlobalState d) m a }
	deriving( MonadTrans, Applicative, Monad, Functor )

instance (Monad m) => MonadState d (SchedulerMonadT d m) where
	get = SchedulerMonadT $ getSchedData
	put = SchedulerMonadT . putSchedData

instance (MonadLog m) => SchedulerMonad d (SchedulerMonadT d m) where
	setTimer time callerInfo =
		SchedulerMonadT $ withGodState $ modify $
		godState_addEvent time (TimerEvent callerInfo)

	takeCall callerInfo =
		SchedulerMonadT $
		getSimState >>= \simState -> 
			case SimState.serveCall callerInfo simState of
				Nothing -> return False
				Just newState -> putSimState newState >> return True

	takeCallsWhilePossible =
		SchedulerMonadT $
		withSimState $ SimState.serveCallsWhilePossible

	takeNextCall =
		SchedulerMonadT $
		withSimState $ state SimState.serveNextCall

instance (MonadLog m) => MonadLog (SchedulerMonadT d m) where
	doLog str = SchedulerMonadT $ doLog str

-- history:
addToHistory :: Monad m => CallerInfo -> HistoryEntry -> SimulationMonadT (GlobalState schedData) m ()
addToHistory callerInfo entry =
	modify $ glob_mapToHistory $ addEntry callerInfo entry

getHistory :: Monad m => SimulationMonadT (GlobalState schedData) m History
getHistory = gets glob_history

--
withGodState :: Monad m => StateT GodState m a -> SimulationMonadT (GlobalState schedData) m a 
withGodState f =
	get >>= \glob@GlobalState{ glob_god = x } ->
		do
			(ret, x') <- lift $ runStateT f x
			put glob{ glob_god = x' }
			return ret

withSchedulerData :: Monad m => SchedulerMonadT schedData m a -> SimulationMonadT (GlobalState schedData) m a 
withSchedulerData =
	fromSchedulerMonadT

withSimState :: Monad m => StateT SimulationState m a -> SimulationMonadT (GlobalState schedData) m a 
withSimState f =
	get >>= \glob@GlobalState{ glob_simState = x } ->
		do
			(ret, x') <- lift $ runStateT f x
			put glob{ glob_simState = x' }
			return ret


getGodState :: Monad m => SimulationMonadT (GlobalState schedData) m GodState
getGodState = gets glob_god
getSchedData :: Monad m => SimulationMonadT (GlobalState schedData) m schedData
getSchedData = gets glob_schedData
getSimState :: Monad m => SimulationMonadT (GlobalState schedData) m SimulationState
getSimState = gets glob_simState

modifyGodState :: Monad m => (GodState -> GodState) -> SimulationMonadT (GlobalState schedData) m ()
modifyGodState = modify . glob_mapToGod

modifySchedData :: Monad m => (schedData -> schedData) -> SimulationMonadT (GlobalState schedData) m ()
modifySchedData = modify . glob_mapToSchedData

modifySimState :: Monad m => (SimulationState -> SimulationState) -> SimulationMonadT (GlobalState schedData) m ()
modifySimState = modify . glob_mapToSimState

putSimState x = modifySimState $ const x
putSchedData x = modifySchedData $ const x
putGodState x = modifyGodState $ const x
