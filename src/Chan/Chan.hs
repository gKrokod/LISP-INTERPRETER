{-# LANGUAGE StandaloneDeriving #-}
module Chan.Chan where
-- import Control.Concurrent.Chan
import Control.Concurrent
import Types
import qualified Data.Map as Map

helloChan :: IO ()
helloChan = putStrLn "Hello CHan"

type Binding = Map.Map Name Value

-- newEmptyMVar :: IO (Mvar a)
-- newMVar :: a -> IO (Mvar a)
-- takeMVar :: MVar a -> IO a
-- putMVar :: MVar -> a -> a -> IO ()
--
--
--
--
--
-- type Frame = Map.Map Name Value
-- type Scope = [Frame]
-- newtype Environment = Environment (MVar Scope)
--
-- type Name = Token -- TSymbol example
-- type Value = Token -- TSymbol example
-- type EvalError = Token
-- type EvalToken = Either EvalError Value

type Scope a = MVar (Frame a) 
-- содержание окружения, по идее список фреймов
data Frame a = Frame a (Scope a) 
  -- фрем состоит из области видимости и MVar на новую область видимости.
data Environment a = Env (MVar (Scope a))
-- наше окружение которое есть ящик содержащий, другой 
-- ящик с таблицей биндингов и новым ящиком
--
newEnvironment :: IO (Environment a)
newEnvironment = do
  hole <- newEmptyMVar -- создаем пустой scope , а потом создаем окружением, которое будет ссылаться на наш пустой scope
  newBox <- newMVar hole  -- создаем окружение в котором ссылка на пуской scope
  pure $ Env newBox


addEnvironment :: Environment a -> a -> IO (Environment a)
addEnvironment (Env scope) binding = do
  newHole <- newEmptyMVar
  oldHole <- takeMVar scope
  putMVar oldHole (Frame binding newHole)
  putMVar scope newHole
  pure $ Env scope


chan :: IO ()
chan = do
  e1 <- newEnvironment
  e2 <- addEnvironment e1 [2]
  print "hello"
  -- print $ e2

