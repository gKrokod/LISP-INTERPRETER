module Scope where

import Types

import Control.Concurrent --(MVar, newMVar, putMVar, takeMVar)
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
-- type Binding = Map.Map Name Value
-- -- окружение есть ящик содержащий фрейm
-- type Environment a = MVar (Frame a) 
-- -- фрейм из ящика окружения есть таблица связывания (Frame a)
-- -- и новый ящик и объемлющего окружения (enclosing environment) 
-- data Frame a = Frame a (Environment a) 
-- -- Создаем родительской окружение, которое никуда не сылается, а содержит MVar ()

-- type Environment a = MVar (Frame a) 

createEnvironment :: IO (Environment a)
createEnvironment = newEmptyMVar 
--
-- Создаем локальное окружение на основе объемлющего и таблицы биндингов
makeLocalEnvironment :: Environment a -> a -> IO (Environment a)
makeLocalEnvironment env binding = newMVar (Frame binding env)

-- Пробегаемся по всем скоупам и выводим биндинги наши
printAllEnvironment :: Environment a -> IO [a]
printAllEnvironment env = do
  isEmpty <- tryTakeMVar env
  case isEmpty of
    Nothing -> do 
      -- putMVar m ()
      -- возможно тут надо делать пут? хотя после тейк пустого места, там и должно быть пустоло
      pure []
    Just (Frame b env') -> do
      putMVar env (Frame b env')
      fmap (b :) (printAllEnvironment env') 

-- data Handle m a = Handle
--   -- {   eval :: EvalToken -> m (EvalToken) 
--     { writeLog :: String -> m ()
--     , check :: Environment a -> Name -> m (Either EvalError Value)
--     , update :: Environment a -> Name -> Value -> m ()
--     , insert :: Environment a -> Name -> Value -> m ()
--     , funcBOMUL :: Value -> Value -> Value
--     , funcBOADD :: Value -> Value -> Value
--     , funcBOSUB :: Value -> Value -> Value
--     , funcBODIV :: Value -> Value -> Value
--     , funcBOMOD :: Value -> Value -> Value
--     , funcBOCONCAT :: Value -> Value -> Value
--     , funcBPGT :: Value -> Value -> Value
--     , funcBPLT :: Value -> Value -> Value
--     , funcBPEQ :: Value -> Value -> Value
--     , funcSFTYPEOF :: Value -> Value
--     , hPrint :: EvalToken -> m ()
--     , hRead :: m (EvalToken)
--     , environment :: Environment a
--     -- , makeLocalEnvironment :: Environment a -> a -> m (Environment a)
--   }

-- newScope :: IO ScopeGlobal
-- newScope = do
--   m <- newMVar $ Map.fromList [(TSymbol "pi", TDouble 3.14)] 
--   pure $ ScopeGlobal m
--
check :: Environment Binding -> Name -> IO (EvalToken)
check env name = do
  isEmpty <- tryTakeMVar env
  case isEmpty of
    Nothing -> do 
      -- putMVar m ()
      -- возможно тут надо делать пут? хотя после тейк пустого места, там и должно быть пустоло
      pure $ Left $ TEvalError (show name ++ " does not exist in scope")
    Just (Frame binding env') -> do
      putMVar env (Frame binding env')
      case Map.lookup name binding of
        Nothing -> check env' name
        Just v -> pure $ Right v

update :: Environment Binding -> Name -> Value -> IO ()
update env name value = insert env name value 

insert :: Environment Binding -> Name -> Value -> IO ()
insert env name value = do 
  isEmpty <- tryTakeMVar env
  case isEmpty of
    Nothing -> do 
      -- putMVar m ()
      -- возможно тут надо делать пут? хотя после тейк пустого места, там и должно быть пустоло
      -- putMVar env (Frame b env')
      newEmpty <-newEmptyMVar
      putMVar env (Frame (Map.fromList [(name, value)]) newEmpty) 
      -- pure $ Right $ TNil
     -- pure $ Left $ TEvalError (show name ++ " does not exist in scope")
    Just (Frame binding env') -> do
      let binding' = Map.insert name value binding
      putMVar env (Frame binding' env')
      -- seq binding' (pure $ Right $ TNil)

  -- scope <- takeMVar m
  -- putMVar m scope
  -- case Map.lookup name scope of
  --   Nothing -> pure $ Left $ TEvalError (show name ++ " does not exist in scope")
  --   Just v -> pure $ Right v
--   
-- update :: ScopeGlobal -> Name -> Value -> IO ()
-- update (ScopeGlobal m) name value = do
--   scope <- takeMVar m
--   let scope' = Map.insert name value scope
--   putMVar m scope'
--   -- for test L.writeFile "config/base.db" (LC.pack $ showTree base')
--   seq scope' (pure ())
--
-- scopeGlobal = [Frame] = [M
-- type Frame = Map.Map Name Value
-- type Scope = [Frame]
-- newtype Environment = Environment (MVar Scope)

-- newEnvironment :: IO Environment
-- newEnvironment = do
--   m <- newMVar $ [Map.fromList [(TSymbol "pi", TDouble 4)] ]
--   pure $ Environment m
--
-- insert :: Environment -> Name -> Value -> IO ()
-- insert (Environment m) name value = do
--   (s : scope) <- takeMVar m
--   let s' = Map.insert name value s
--   putMVar m (s' : scope)
--   seq s' (pure ())
-- -- работа set по изменению переменной в ближайшем скоупе
-- -- update' :: Environment-> Name -> Value -> IO (EvalToken)
-- update' :: Environment-> Name -> Value -> IO ()
-- update' (Environment m) name value = do
--   scope <- takeMVar m
--   case find (Map.member name) scope of
--     Nothing -> pure ()
--     -- Nothing -> pure $ Left $ TEvalError (show name ++ " does not exist in Environment.. set ->")
--     Just _ -> do
--       let scope' = set' scope name value
--       putMVar m (scope')
--       seq scope' (pure ())
--       pure () 
--       -- pure $ Right $ TNil
--
-- -- изменяем значение, но не сообщаем если не нашлилоЖц
-- -- 
-- set' :: Scope -> Name -> Value -> Scope
-- set' [] _ _ = [] --TEvalError (show name ++ " does not exist in Environment.. set ->")
-- set' (x : xs) name value | Map.member name x = (Map.insert name value x) : set' xs name value
--                          | otherwise = x : set' xs name value
--
-- check' :: Environment-> Name -> IO (EvalToken)
-- check' (Environment m) name = do
--   scope <- takeMVar m
--   putMVar m scope
--   case find (Map.member name) scope of
--     Nothing -> pure $ Left $ TEvalError (show name ++ " does not exist in scope 1")
--     Just map -> case Map.lookup name map of 
--       Nothing -> pure $ Left $ TEvalError (show name ++ " does not exist in scope 2")
--       Just v -> pure $ Right v
