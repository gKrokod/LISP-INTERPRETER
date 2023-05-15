module Scope.Scope where
import Types

import Control.Concurrent --(MVar, newMVar, putMVar, takeMVar)
import qualified Data.Map.Strict as Map
import Data.List (find)
--данные в окружении хранятся в таблицах Binding, где ключом является
-- Атом символ, а значение любое s выражение.
-- type Binding = Map.Map Name Value
-- type Name = String --SExpr --Atom String 
-- type Value = SExpr

createEnvironment :: IO (Environment' a)
createEnvironment = newEmptyMVar 
--
-- Создаем локальное окружение на основе объемлющего и таблицы биндингов
makeLocalEnvironment :: Environment' a -> a -> IO (Environment' a)
makeLocalEnvironment env binding = newMVar (Frame binding env)

fullLocalEnvironment :: (Monoid a) => Environment' a -> a -> IO ()
fullLocalEnvironment env binding = do 
  isEmpty <- tryTakeMVar env
  case isEmpty of
    Nothing -> do 
      -- putMVar m ()
      -- возможно тут надо делать пут? хотя после тейк пустого места, там и должно быть пустоло
      -- putMVar env (Frame b env')
      newEmpty <-newEmptyMVar
      putMVar env (Frame binding newEmpty) 
    Just (Frame bindingOld env') -> do
      let binding' = binding <> bindingOld
      putMVar env (Frame binding' env')
      seq binding' (pure ())
      -- seq binding' (pure $ Right $ TNil)

-- Пробегаемся по всем скоупам и выводим биндинги наши
printAllEnvironment :: Environment' a -> IO [a]
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
--
check :: Environment -> Name -> IO (Maybe SExpr)
check env name = do
  isEmpty <- tryTakeMVar env
  case isEmpty of
    Nothing -> do 
      pure $ Nothing
    Just (Frame binding env') -> do
      putMVar env (Frame binding env')
      case Map.lookup name binding of
        Nothing -> check env' name
        Just value -> pure $ Just value
--
-- insert :: Environment Binding -> Name -> Value -> IO ()
insert :: Environment -> Name -> SExpr -> IO ()
insert env name value = do 
  isEmpty <- tryTakeMVar env
  case isEmpty of
    Nothing -> do 
      -- putMVar m ()
      -- возможно тут надо делать пут? хотя после тейк пустого места, там и должно быть пустоло
      -- putMVar env (Frame b env')
      newEmpty <-newEmptyMVar
      putMVar env (Frame (Map.fromList [(name, value)]) newEmpty) 
    Just (Frame binding env') -> do
      let binding' = Map.insert name value binding
      putMVar env (Frame binding' env')
      seq binding' (pure ())
      -- seq binding' (pure $ Right $ TNil)
      
update :: Environment -> Name -> SExpr -> IO (SExpr) -- Bool Bool -> #f, #t
update env name value = do
  isEmpty <- tryTakeMVar env
  case isEmpty of
    Nothing -> do 
      pure $ Bool False
    Just (Frame binding env') -> do
      -- putMVar env (Frame binding env')
      case Map.lookup name binding of
        Nothing -> do
          putMVar env (Frame binding env')
          update env' name value
        Just _ -> do
          let binding' = Map.insert name value binding
          putMVar env (Frame binding' env')
          seq binding' (pure $ Bool True)
