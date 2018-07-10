module Utils where

import Control.Monad.State


modifyMaybe ::
	MonadState s m =>
	(s -> Maybe s) -> m ()
modifyMaybe f =
	modify $
	\oldVal ->
	case f oldVal of
		Nothing -> oldVal
		Just newVal -> newVal
