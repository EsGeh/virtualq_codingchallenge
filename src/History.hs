{-# LANGUAGE FlexibleContexts #-}
module History where

import Types

import qualified Data.Map as M


-- type History = [HistoryEntry]

type History = M.Map CallerInfo CallerHistory

type CallerHistory = [HistoryEntry]

data HistoryEntry
	= HistoryEntry{
		history_time :: Time,
		history_event :: EventType
		-- history_caller :: CallerInfo
	}
	deriving( Show, Read, Eq, Ord)

data EventType
	= IncomingCallEvent
	| HangupCallEvent
	| ServeCallEvent
	deriving( Show, Read, Eq, Ord)


addEntry :: CallerInfo -> HistoryEntry -> History -> History
addEntry callerInfo entry hist =
	M.alter updateF callerInfo hist
	where
		updateF mOldHist =
			case mOldHist of
				Nothing -> Just $ [entry]
				Just callerHist ->
					Just $ entry : callerHist
