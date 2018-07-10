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


-- number of latest entries taken into account for calculations:
historyLength = 5


-- TODO: take into account current time!

calcAvgWaitingTime :: History -> Float
calcAvgWaitingTime =
	avg
	. map (uncurry calcWaitingTime)
	. take historyLength
	. reverse . sortOn snd -- sort latest to earliest serve time
	. catMaybes
	. map getCallTimeAndServeTime
	. M.elems

calcLongestWaitingTime :: History -> Float
calcLongestWaitingTime =
	maximum
	. map (uncurry calcWaitingTime)
	. take historyLength
	. reverse . sortOn snd -- sort latest to earliest serve time
	. catMaybes
	. map getCallTimeAndServeTime
	. M.elems

calcAvgServeTime :: History -> Float
calcAvgServeTime =
	avg
	. diffBetweenNeighbours
	. take historyLength  -- only take into account the latest entries
	. reverse . sort -- sort latest to earliest
	. catMaybes
	. map (fmap snd . getCallTimeAndServeTime)
	. M.elems

calcWaitingTime :: Time -> Time -> Time
calcWaitingTime callTime serveTime = serveTime - callTime

getCallTime :: CallerHistory -> Maybe Time
getCallTime events =
	history_time <$>
	(find ((/=IncomingCallEvent) . history_event) $ events)

getCallTimeAndServeTime :: CallerHistory -> Maybe (Time, Time)
getCallTimeAndServeTime events =
	case dropWhile ((/=ServeCallEvent) . history_event) events of
		[] -> Nothing
		serveCallEvent:previousEvents ->
			do
				incomingCallEvent <- find ((==IncomingCallEvent) . history_event) previousEvents
				return (history_time incomingCallEvent, history_time serveCallEvent)

avg :: [Float] -> Float
avg values =
	sum values / fromIntegral (length values)

diffBetweenNeighbours :: [Float] -> [Float]
diffBetweenNeighbours l =
	case l of
		[] -> []
		(x:[]) -> []
		(x:xs:rest) -> (x - xs) : diffBetweenNeighbours (xs:rest)
