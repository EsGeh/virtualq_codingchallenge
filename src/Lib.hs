{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib(
	runSimulation
) where

import qualified Data.Map as M
import Control.Monad.Random
import Control.Monad.State


-- |Queue of callers
type CallerQ = [QueueEntry]

-- |Information about a caller
data QueueEntry = QueueEntry

type TimeQ a = M.Map Time a

-- |time in hours
type Time = Float

data SimulationState
	= SimulationState {
		-- simState_callerQ :: CallerQ
	}


runSimulation :: IO ()
runSimulation =
	do
		_ <- evalStateT (runMainLoop 0) initState
		return ()
	where
		initState = SimulationState

runMainLoop :: (MonadState SimulationState m, MonadRandom m) => Time -> m ()
runMainLoop t =
	do
		futureCalls <- spawnCallers 100 t (t+1)
		modify (simulateOneHour futureCalls)
		return ()

simulateOneHour :: TimeQ () -> SimulationState -> SimulationState
simulateOneHour futureCalls simState =
	simState


--------------------------------------------------
-- helper functions:
--------------------------------------------------

spawnCallers :: forall m . (MonadRandom m) => Float -> Time -> Time -> m (TimeQ ())
spawnCallers density start end =
	let
		number = ceiling $ (end-start)*density
	in
		do
			eventTimes <-
				take number <$> getRandomRs (start, end)
				:: m [Time]
			return $ M.fromList $ eventTimes `zip` repeat ()
