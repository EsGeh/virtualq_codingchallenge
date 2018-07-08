module SimulationMonad where

import Types

import Control.Monad.State


data GodState
	= GodState {
		godState_upcomingCalls :: TimeQ CallerInfo,
		godState_callHangups :: TimeQ CallerInfo,
		godState_counter :: Int
	}
	deriving( Show, Read, Eq, Ord)

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
