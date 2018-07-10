module Analysis(
	calcAvgWaitingTime,
	calcLongestWaitingTime,
	calcAvgServeTime,
) where

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

calcLongestWaitingTime :: History -> Float
calcLongestWaitingTime =
	maximum
	. catMaybes
	. map calcWaitingTime
	. M.elems

-- TODO: Fix!
calcAvgServeTime :: History -> Float
calcAvgServeTime =
	avg
	. diffBetweenNeighbours
	. sort
	. catMaybes
	. map (fmap snd . getCallAndHangupTime)
	. M.elems

calcWaitingTime :: CallerHistory -> Maybe Time
calcWaitingTime =
	fmap (uncurry (-))
	.
	getCallAndHangupTime

getCallAndHangupTime events =
	case dropWhile ((/=ServeCallEvent) . history_event) events of
		[] -> Nothing
		serveCallEvent:previousEvents ->
			do
				incomingCallEvent <- find ((==IncomingCallEvent) . history_event) previousEvents
				return (history_time serveCallEvent, history_time incomingCallEvent)

avg :: [Float] -> Float
avg values =
	sum values / fromIntegral (length values)

diffBetweenNeighbours :: [Float] -> [Float]
diffBetweenNeighbours l =
	case l of
		[] -> []
		(x:[]) -> []
		(x:xs:rest) -> (xs - x) : diffBetweenNeighbours (xs:rest)
