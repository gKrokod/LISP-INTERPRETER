module Handlers.Eval where
import qualified Handlers.Scope

data Handle m a = Handle {
  scope :: Handlers.Scope.Handle m a
  logger :: Handlers.Logger.Handle m
}

-- data Handle m = Handle
--   { client :: Handlers.Client.Handle m,
--     bot :: Handlers.Bot.Handle m,
--     logger :: Handlers.Logger.Handle m,
--     forkForUser :: m () -> m ()
--   }
