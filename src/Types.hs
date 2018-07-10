{-# LANGUAGE RecordWildCards #-}
module Types where

import qualified Data.Map as M


type CallerId = Int

-- |time in hours
type Time = Float

type TimeQ a = M.Map Time a

-- |Information about a caller
data CallerInfo = CallerInfo CallerId
	deriving( Show, Read, Eq, Ord)

data Event
	= IncomingCall CallerInfo
	| HangupCall CallerInfo
	| TimerEvent CallerInfo
	deriving( Show, Read, Eq, Ord)

minute :: Float
minute = 1 / 60
