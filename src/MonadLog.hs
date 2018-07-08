module MonadLog where


import System.IO
import Control.Monad.State

class (Monad m) => MonadLog m where
	doLog :: String -> m ()

instance MonadLog IO where
	doLog = putStrLn

instance (MonadLog m) => MonadLog (StateT s m) where
	doLog = lift . doLog
