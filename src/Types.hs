{-# LANGUAGE RecordWildCards #-}
module Types where

import qualified Data.Map as M


-- |Queue of callers
type CallerQ = [CallerInfo]

-- |Information about a caller
data CallerInfo = CallerInfo CallerId
	deriving( Show, Read, Eq, Ord)

type CallerId = Int

-- |time in hours
type Time = Float

type TimeQ a = M.Map Time a

data Event
	= IncomingCall CallerInfo
	| HangupCall CallerInfo
	| TimerEvent CallerInfo
	deriving( Show, Read, Eq, Ord)
