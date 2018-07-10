{-# LANGUAGE RecordWildCards #-}
module Types where

import qualified Data.Map as M


-- |uniquely identify a caller
type CallerId = Int

-- |time in hours
type Time = Float

-- |events of type `a` ordered in time
type TimeQ a = M.Map Time a

-- |Information about a caller
data CallerInfo = CallerInfo CallerId
	deriving( Show, Read, Eq, Ord)

-- |events that can occur in the simulation scenario
data Event
	= IncomingCall CallerInfo
	| HangupCall CallerInfo
	| TimerEvent CallerInfo
	deriving( Show, Read, Eq, Ord)

minute :: Float
minute = 1 / 60
