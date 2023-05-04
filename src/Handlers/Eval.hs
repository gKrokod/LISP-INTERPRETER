module Handlers.Eval where
import qualified Handlers.Scope as S
import qualified Handlers.Logger as L
import qualified Handlers.Logger
import qualified Handlers.Scope
import Types

data Handle m a = Handle {
    scope :: Handlers.Scope.Handle m a
  , logger :: Handlers.Logger.Handle m
}

eval :: (Monad m) => Handle m a -> Environment a -> SExpr -> m (SExpr)
eval h env expr@(Number _) = do
  L.writeLog (logger h) "read Number" 
  pure expr
eval h env expr@(String _) = do 
  L.writeLog (logger h) "read String" 
  pure expr
eval h env expr@(Bool _) = do 
  L.writeLog (logger h) "read Bool" 
  pure expr
eval h env (List [Atom "quote", val]) = do
  L.writeLog (logger h) "read quote" 
  pure val
