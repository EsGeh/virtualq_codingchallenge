module SchedulerBase where

import SchedulerAPI


schedLog :: MonadLog m => String -> m ()
schedLog msg = doLog $ "\tsched: " ++ msg
