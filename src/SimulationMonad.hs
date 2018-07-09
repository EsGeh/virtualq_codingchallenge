{-# LANGUAGE RecordWildCards #-}
module SimulationMonad where

import Types
-- import Analytics

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Monad.Identity


data GodState
	= GodState {
		godState_upcomingEvents :: TimeQ GodEvent,
		godState_counter :: Int
	}
	deriving( Show, Read, Eq, Ord)

data GodEvent
	= IncomingCall CallerInfo
	| HangupCall CallerInfo
	deriving( Show, Read, Eq, Ord)

getNextEvent :: Monad m =>
	StateT GodState m (Maybe (Time, GodEvent))
getNextEvent =
	get >>= \godState@GodState{..} ->
		return $ M.lookupMin godState_upcomingEvents

popNextEvent :: Monad m =>
	StateT GodState m (Maybe (Time, GodEvent))
popNextEvent =
	do
		next <- getNextEvent
		modify $ \godState@GodState{..} ->
			godState{ godState_upcomingEvents = M.deleteMin godState_upcomingEvents }
		return next

godStateInit = GodState {
	godState_upcomingEvents = M.empty,
	godState_counter = 0
}

data GlobalState
	= GlobalState {
		glob_god :: GodState,
		glob_simState :: SimulationState
		-- glob_log :: LoggerData
}

runSimulationMonad initState =
	evalStateT `flip` init
	where
		init = GlobalState{
			glob_god = godStateInit,
			glob_simState = initState
			-- glob_log = [] 
		}

glob_mapToGod f = runIdentity . glob_mapToGodM (return . f)
glob_mapToGodM :: Monad m => (GodState -> m GodState) -> GlobalState -> m GlobalState
glob_mapToGodM f x =
	do
		val <- f $ glob_god x
		return $ x{ glob_god = val }

glob_mapToSimState f = runIdentity . glob_mapToSimStateM (return . f)

glob_mapToSimStateM :: Monad m => (SimulationState -> m SimulationState) -> GlobalState -> m GlobalState
glob_mapToSimStateM f x =
	do
		val <- f $ glob_simState x
		return $ x{ glob_simState = val }


type SimulationMonadT s m a = StateT s m a
-- (read, write, state)

{-
addLoggerEntry :: Monad m => LoggerEntry -> SimulationMonadT GlobalState m ()
addLoggerEntry entry =
	modify $ \
-}

withGodState :: Monad m => StateT GodState m a -> SimulationMonadT GlobalState m a 
withGodState f =
	get >>= \glob@GlobalState{ glob_god = x } ->
		do
			(ret, x') <- lift $ runStateT f x
			put glob{ glob_god = x' }
			return ret

withSimState :: Monad m => StateT SimulationState m a -> SimulationMonadT GlobalState m a 
withSimState f =
	get >>= \glob@GlobalState{ glob_simState = x } ->
		do
			(ret, x') <- lift $ runStateT f x
			put glob{ glob_simState = x' }
			return ret

getGodState :: Monad m => SimulationMonadT GlobalState m GodState
getGodState = gets glob_god
getSimState :: Monad m => SimulationMonadT GlobalState m SimulationState
getSimState = gets glob_simState

modifyGodState :: Monad m => (GodState -> GodState) -> SimulationMonadT GlobalState m ()
modifyGodState = modify . glob_mapToGod

modifySimState :: Monad m => (SimulationState -> SimulationState) -> SimulationMonadT GlobalState m ()
modifySimState = modify . glob_mapToSimState

putSimState x = modifySimState $ const x
putGodState x = modifyGodState $ const x

mapFst f (a, b) = (f a, b)
mapSnd f (a, b) = (a, f b)
