module Danac.Util.Helpers where

import Control.Monad.State.Strict (MonadState, put, get)

locally :: MonadState s m => m a -> m a
locally computation = do
  oldState <- get
  result   <- computation
  put oldState
  return result
