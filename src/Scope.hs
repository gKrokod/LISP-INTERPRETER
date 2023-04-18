module Scope where

import Types

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import qualified Data.Map.Strict as Map
import Types
import Data.List (find)

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
    Just v -> pure $ Right v
  
update :: ScopeGlobal -> Name -> Value -> IO ()
update (ScopeGlobal m) name value = do
  scope <- takeMVar m
  let scope' = Map.insert name value scope
  putMVar m scope'
  -- for test L.writeFile "config/base.db" (LC.pack $ showTree base')
  seq scope' (pure ())

-- scopeGlobal = [Frame] = [M
type Frame = Map.Map Name Value
type Scope = [Frame]
newtype Environment = Environment (MVar Scope)

insert :: Environment-> Name -> Value -> IO ()
insert (Environment m) name value = do
  (s : scope) <- takeMVar m
  let s' = Map.insert name value s
  putMVar m (s' : scope)
  seq s' (pure ())
-- работа set по изменению переменной в ближайшем скоупе
update' :: Environment-> Name -> Value -> IO (EvalToken)
update' (Environment m) name value = do
  scope <- takeMVar m
  case find (Map.member name) scope of
    Nothing -> pure $ Left $ TEvalError (show name ++ " does not exist in Environment.. set ->")
    Just _ -> do
      let scope' = set' scope name value
      putMVar m (scope')
      seq scope' (pure ())
      pure $ Right $ TNil

-- изменяем значение, но не сообщаем если не нашлилоЖц
-- 
set' :: Scope -> Name -> Value -> Scope
set' [] _ _ = [] --TEvalError (show name ++ " does not exist in Environment.. set ->")
set' (x : xs) name value | Map.member name x = (Map.insert name value x) : set' xs name value
                         | otherwise = x : set' xs name value

check' :: Environment-> Name -> IO (EvalToken)
check' (Environment m) name = do
  scope <- takeMVar m
  putMVar m scope
  case find (Map.member name) scope of
    Nothing -> pure $ Left $ TEvalError (show name ++ " does not exist in scope 1")
    Just map -> case Map.lookup name map of 
      Nothing -> pure $ Left $ TEvalError (show name ++ " does not exist in scope 2")
      Just v -> pure $ Right v
