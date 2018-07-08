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

getNextUpcomingCall :: Monad m =>
	StateT GodState m (Maybe (Time, CallerInfo))
getNextUpcomingCall =
	get >>= \godState@GodState{..} ->
	do
		let mNextCall = M.minViewWithKey godState_upcomingCalls
		case mNextCall of
			Nothing -> return Nothing
			Just (nextCall, futureCallsRest) ->
				do
					put $ godState{ godState_upcomingCalls = futureCallsRest }
					return $ Just nextCall

getNextEndCall 
	get >>= \godState@GodState{..} ->
	do
		let mNext = M.minViewWithKey godState_callEndTimes
		case mNextCall of
			Nothing -> return Nothing
			Just (nextEvent, remainingEvents) ->
				do
					put $ godState{ godState_callEndTimes = remainingEvents }
					return $ Just nextEvent

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
