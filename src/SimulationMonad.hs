{-# LANGUAGE RecordWildCards #-}
module SimulationMonad where

import Types

import qualified Data.Map as M
import Control.Monad.State


data GodState
	= GodState {
		godState_upcomingCalls :: TimeQ CallerInfo,
		godState_callEndTimes :: TimeQ CallerInfo,
		godState_counter :: Int
	}
	deriving( Show, Read, Eq, Ord)

godStateInit = GodState {
	godState_upcomingCalls = M.empty,
	godState_callEndTimes = M.empty,
	godState_counter = 0
}

type SimulationMonadT m a = StateT GodState (StateT SimulationState m) a

getNextCall :: Monad m =>
	StateT GodState m (Maybe (Time, CallerInfo))
getNextCall =
	get >>= \godState@GodState{..} ->
		return $ M.lookupMin godState_upcomingCalls

getNextEndCall :: Monad m =>
	StateT GodState m (Maybe (Time, CallerInfo))
getNextEndCall =
	get >>= \godState@GodState{..} ->
		return $ M.lookupMin godState_callEndTimes

popNextCall :: Monad m =>
	StateT GodState m (Maybe (Time, CallerInfo))
popNextCall =
	do
		next <- getNextCall
		modify $ \godState@GodState{..} ->
			godState{ godState_upcomingCalls = M.deleteMin godState_upcomingCalls }
		return next

popNextEndCall :: Monad m =>
	StateT GodState m (Maybe (Time, CallerInfo))
popNextEndCall =
	do
		next <- getNextEndCall
		modify $ \godState@GodState{..} ->
			godState{ godState_callEndTimes = M.deleteMin godState_callEndTimes }
		return next

{-
popNextEndCall =
	get >>= \godState@GodState{..} ->
	do
		let mNext = M.minViewWithKey godState_callEndTimes
		case mNextCall of
			Nothing -> return Nothing
			Just (nextEvent, remainingEvents) ->
				do
					put $ godState{ godState_callEndTimes = remainingEvents }
					return $ Just nextEvent
-}

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
