{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SimulationMonad where

import Types
import History
import SchedulerAPI

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Monad.Identity


-- global simulation state
data GlobalState s
	= GlobalState {
		glob_god :: GodState,
		glob_simState :: s,
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
glob_mapToGodM :: Monad m => (GodState -> m GodState) -> (GlobalState s) -> m (GlobalState s)
glob_mapToGodM f x =
	do
		val <- f $ glob_god x
		return $ x{ glob_god = val }

glob_mapToSimState f = runIdentity . glob_mapToSimStateM (return . f)

glob_mapToSimStateM :: Monad m => (s -> m s) -> GlobalState s -> m (GlobalState s)
glob_mapToSimStateM f x =
	do
		val <- f $ glob_simState x
		return $ x{ glob_simState = val }

glob_mapToHistory f = runIdentity . glob_mapToHistoryM (return . f)

glob_mapToHistoryM :: Monad m => (History -> m History) -> GlobalState s -> m (GlobalState s)
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

type SimulationMonadT s m a = StateT s m a
-- (read, write, state)

runSimulationMonad initState =
	evalStateT `flip` init
	where
		init = GlobalState{
			glob_god = godStateInit,
			glob_simState = initState,
			glob_history = M.empty
		}

-- this is the monad in which the scheduler runs:
newtype SchedulerMonadT d m a = SchedulerMonadT { fromSchedulerMonadT :: StateT (GlobalState d) m a }
	deriving( MonadTrans, Applicative, Monad, Functor )

instance (Monad m) => MonadState d (SchedulerMonadT d m) where
	get = SchedulerMonadT $ getSimState
	put = SchedulerMonadT . putSimState

instance (MonadLog m) => SchedulerMonad d (SchedulerMonadT d m) where
	setTimer time callerInfo =
		SchedulerMonadT $ withGodState $ modify $
		godState_addEvent time (TimerEvent callerInfo)

instance (MonadLog m) => MonadLog (SchedulerMonadT d m) where
	doLog str = SchedulerMonadT $ doLog str

-- history:
addToHistory :: Monad m => CallerInfo -> HistoryEntry -> SimulationMonadT (GlobalState s) m ()
addToHistory callerInfo entry =
	modify $ glob_mapToHistory $ addEntry callerInfo entry

getHistory :: Monad m => SimulationMonadT (GlobalState s) m History
getHistory = gets glob_history

--
withGodState :: Monad m => StateT GodState m a -> SimulationMonadT (GlobalState s) m a 
withGodState f =
	get >>= \glob@GlobalState{ glob_god = x } ->
		do
			(ret, x') <- lift $ runStateT f x
			put glob{ glob_god = x' }
			return ret

withSimState :: Monad m => SchedulerMonadT s m a -> SimulationMonadT (GlobalState s) m a 
withSimState =
	fromSchedulerMonadT
{-
	get >>= \glob@GlobalState{ glob_simState = x } ->
		do
			(ret, x') <- lift $ runStateT (fromSchedulerMonadT f) x
			put glob{ glob_simState = x' }
			return ret
-}

getGodState :: Monad m => SimulationMonadT (GlobalState s) m GodState
getGodState = gets glob_god
getSimState :: Monad m => SimulationMonadT (GlobalState s) m s
getSimState = gets glob_simState

modifyGodState :: Monad m => (GodState -> GodState) -> SimulationMonadT (GlobalState s) m ()
modifyGodState = modify . glob_mapToGod

modifySimState :: Monad m => (s -> s) -> SimulationMonadT (GlobalState s) m ()
modifySimState = modify . glob_mapToSimState

putSimState x = modifySimState $ const x
putGodState x = modifyGodState $ const x
