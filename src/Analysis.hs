module Analysis(
	calcAvgWaitingTime,
	calcLongestWaitingTime,
	calcAvgServeTime,
) where

import Types
import History
import qualified Utils

import Data.List
import Data.Maybe
import qualified Data.Map as M


-- number of latest entries taken into account for calculations:
historyLength = 5


calcAvgWaitingTime :: Time -> History -> Float
calcAvgWaitingTime currentTime =
	avg
	. map (uncurry calcWaitingTime)
	. take historyLength
	. reverse . sortOn snd -- sort latest to earliest serve time
	. map (
		Utils.mapSnd (fromMaybe currentTime) 
		. getCallTimeAndServeTime
	) -- if not yet served, assume currentTime
	. M.elems

calcLongestWaitingTime :: Time -> History -> Float
calcLongestWaitingTime currentTime =
	(\l -> if null l then 0 else maximum l)
	. map (uncurry calcWaitingTime)
	. take historyLength
	. reverse . sortOn snd -- sort latest to earliest serve time
	. map (
		Utils.mapSnd (fromMaybe currentTime)
		. getCallTimeAndServeTime
	) -- if not yet served, assume currentTime
	. M.elems

calcAvgServeTime :: Time -> History -> Float
calcAvgServeTime _ =
	avg
	. diffBetweenNeighbours
	. take historyLength  -- only take into account the latest entries
	. reverse . sort -- sort latest to earliest
	. catMaybes
	. map (snd . getCallTimeAndServeTime)
	. M.elems

calcWaitingTime :: Time -> Time -> Time
calcWaitingTime callTime serveTime = serveTime - callTime

{-
getCallTime :: CallerHistory -> Maybe Time
getCallTime events =
	history_time <$>
	(find ((/=IncomingCallEvent) . history_event) $ events)
-}

getCallTimeAndServeTime :: CallerHistory -> (Time, Maybe Time)
getCallTimeAndServeTime events =
	case break ((==IncomingCallEvent) . history_event) events of
		(_,[]) -> error "history corrupted!"
		(laterEvents, incomingCallEvent:_) ->
			( history_time incomingCallEvent
			, fmap history_time $ find ((==ServeCallEvent) . history_event) $ reverse laterEvents
			)

{-
getCallTimeAndServeTime :: CallerHistory -> Maybe (Time, Time)
getCallTimeAndServeTime events =
	case dropWhile ((/=ServeCallEvent) . history_event) events of
		[] -> Nothing
		serveCallEvent:previousEvents ->
			do
				incomingCallEvent <- find ((==IncomingCallEvent) . history_event) previousEvents
				return (history_time incomingCallEvent, history_time serveCallEvent)
-}

avg :: [Float] -> Float
avg values =
	sum values / fromIntegral (length values)

diffBetweenNeighbours :: [Float] -> [Float]
diffBetweenNeighbours l =
	case l of
		[] -> []
		(x:[]) -> []
		(x:xs:rest) -> (x - xs) : diffBetweenNeighbours (xs:rest)
