{-# LANGUAGE RecordWildCards #-}
module SimulationMonad where

import Types

import qualified Data.Map as M
import Control.Monad.State


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
