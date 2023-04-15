module Scope where

import Types

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import qualified Data.Map.Strict as Map
import Types

-- import qualified Data.ByteString.Lazy.Char8 as LC
-- import qualified Data.ByteString.Lazy as L
-- import Data.Map.Internal.Debug (showTree)

-- type Name = Token -- TSymbol example
-- type Value = Token -- TSymbol example
-- type EvalError = Token
-- type EvalToken = Either EvalError Value

type ScopeMap = Map.Map Name Value
newtype ScopeGlobal = ScopeGlobal (MVar ScopeMap)

-- data Handle m = Handle
--   -- {   eval :: EvalToken -> m (EvalToken) 
--     { writeLog :: String -> m ()
--     , check :: Name -> m (EvalToken)
--     , update :: Name -> Value -> m ()
--   }

newScope :: IO ScopeGlobal
newScope = do
  m <- newMVar $ Map.fromList [(TSymbol "pi", TDouble 3.14)] 
  pure $ ScopeGlobal m

check :: ScopeGlobal -> Name -> IO (EvalToken)
check (ScopeGlobal m) name = do
  scope <- takeMVar m
  putMVar m scope
  case Map.lookup name scope of
    Nothing -> pure $ Left $ TEvalError (show name ++ " does not exist in scope")
    Just v -> pure $ Right $ v
  
update :: ScopeGlobal -> Name -> Value -> IO ()
update (ScopeGlobal m) name value = do
  scope <- takeMVar m
  putMVar m scope
  let scope' = Map.insert name value scope
  putMVar m scope'
  -- for test L.writeFile "config/base.db" (LC.pack $ showTree base')
  seq scope' (pure ())


