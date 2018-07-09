module Analysis where

import Types
import History

import Data.List
import Data.Maybe
import qualified Data.Map as M


-- |TODO: only take into account the latest entries
calcAvgWaitingTime :: History -> Float
calcAvgWaitingTime =
	avg
	. catMaybes
	. map calcWaitingTime
	. M.elems

calcWaitingTime :: CallerHistory -> Maybe Time
calcWaitingTime events =
	case dropWhile ((/=ServeCallEvent) . history_event) events of
		[] -> Nothing
		serveCallEvent:previousEvents ->
			do
				incomingCallEvent <- find ((==IncomingCallEvent) . history_event) previousEvents
				return $ history_time serveCallEvent - history_time incomingCallEvent

avg :: [Float] -> Float
avg values =
	sum values / fromIntegral (length values)
