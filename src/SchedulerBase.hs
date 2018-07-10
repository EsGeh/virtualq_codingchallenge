module SchedulerBase where

import SchedulerAPI

import qualified Data.Set as S
import Control.Monad.State
import Data.Maybe
import Data.List


schedLog msg = doLog $ "\tsched: " ++ msg
