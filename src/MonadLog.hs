module MonadLog where


import System.IO
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS


class (Monad m) => MonadLog m where
	doLog :: String -> m ()

instance MonadLog IO where
	doLog = putStrLn

instance (MonadLog m) => MonadLog (StateT s m) where
	doLog = lift . doLog

instance (MonadLog m, Monoid w) => MonadLog (WriterT w m) where
	doLog = lift . doLog

instance (MonadLog m, Monoid w) => MonadLog (RWST r w s m) where
	doLog = lift . doLog
