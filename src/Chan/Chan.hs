module Chan.Chan where
-- import Control.Concurrent.Chan
import Control.Concurrent
import Types
import qualified Data.Map as Map

-- helloChan :: IO ()
-- helloChan = putStrLn "Hello CHan"

-- type Binding = Map.Map Name Value

-- окружение есть ящик содержащий фрейm
-- type Environment a = MVar (Frame a) 
-- -- фрейм из ящика окружения есть таблица связывания (Frame a)
-- -- и новый ящик и объемлющего окружения (enclosing environment) 
-- data Frame a = Frame a (Environment a) 
-- Создаем родительской окружение, которое никуда не сылается, а содержит MVar ()
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

chan :: IO ()
chan = do
  gs <- createEnvironment
  global <- makeLocalEnvironment gs ["global"]
  vvv <- printAllEnvironment global
  print vvv 
  local <- makeLocalEnvironment global ["local"]
  vvv <- printAllEnvironment local
  print vvv 
  anotherlocal <- makeLocalEnvironment global ["anotherlocal"]
  local2 <- makeLocalEnvironment local ["lambda"]
  anotherlocal2 <- makeLocalEnvironment anotherlocal ["anotherlabmda"]
  value <- printAllEnvironment local2
  value2 <- printAllEnvironment anotherlocal2
  -- value <- readAllScope gs 
  print "Value:"
  print value
  print "ANother Value:"
  print value2
  -- value <- readAllScope local
  -- -- value <- readAllScope gs 
  -- print "Value:"
  -- print value
  -- e1 <- newEnvironment
  -- e2 <- addEnvironment e1 [2]
  -- e3 <- addEnvironment e2 [3]
  -- r1 <- readEnv e1
  -- r2 <- readEnv e2
  -- r3 <- readEnv e3

  -- print (r1 :: ([Int]) )
  -- print r2
  -- print r3
  print "Hello"
--  
--   -- print $ e2
--
--
-- readAllScope :: Scope a -> IO ([a])
-- readAllScope m = do
-- cak
--   Frame b m' <- takeMVar m
--   putMVar m (Frame b m')
--   isempty <- isEmptyMVar m'
--   case isempty of
--     True -> print "pusto" >> pure [b] 
--     False ->fmap (b :) (readAllScope m')
-- --fmap (:) (pure b) readAllScope m'
-- newEmptyMVar :: IO (Mvar a)
-- newMVar :: a -> IO (Mvar a)
-- takeMVar :: MVar a -> IO a
-- putMVar :: MVar a -> a -> IO ()
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

-- type Scope a = MVar (Frame a) 
-- -- содержание окружения, по идее список фреймов
-- -- data ScopeOrNull a = Scope a | ()
-- data Frame a = Frame a (Scope a) 
--   -- фрем состоит из области видимости и MVar на новую область видимости.
-- data Environment a = Env (MVar (Scope a))
-- наше окружение которое есть ящик содержащий, другой 
-- ящик с таблицей биндингов и новым ящиком
--
-- newEnvironment :: IO (Environment a)
-- newEnvironment = do
--   hole <- newEmptyMVar -- Scope a = MVar () создаем пустой scope MVar (),  ::
--   newBox <- newMVar hole  -- Environment a = MVar ( MVar ())  MVсоздаем окружение в котором ссылка на пустой scope
--   pure $ Env newBox
--
--
-- -- добавим новый элемент в наше окружение Environment a = MVar ( MVar ()) -> a -> Env MVar (MVar (Frame a (MVar ()))
-- addEnvironment :: Environment a -> a -> IO ()
-- addEnvironment (Env scope) binding = do
--   -- localScope <- newEmptyMVar
--   -- scope' <- takeMVar scope
--   -- putMVar localScope (Frame binding scope')
--   -- putMVar scope scope'
--   -- pure $ Env scope 
--   newHole <- newEmptyMVar -- new MVarkkkk
--   oldHole <- takeMVar scope -- MVar (Mvar ()) = MVar (Scope a)
--   putMVar oldHole (Frame binding newHole)
--   putMVar scope newHole
--   -- pure $ Env scope

-- readEnv :: Environment a -> IO a
-- readEnv (Env scope) = do
--   scope' <- takeMVar scope
--   Frame v tail <- takeMVar scope'
--   putMVar scope' (Frame v tail)
--   putMVar scope scope'
--   pure v
--
--
-- newScope :: IO (Scope a)
-- newScope = do
--   globalScope <- newEmptyMVar
--   pure $ globalScope
--
-- makeLocalScope :: Scope a -> a -> IO (Scope a)
-- makeLocalScope m binding = do
--   -- Frame b env <- takeMVar m
--   newMVar (Frame binding m)
--
--
-- readAllScope :: Scope a -> IO ([a])
-- readAllScope m = do
--   Frame b m' <- takeMVar m
--   putMVar m (Frame b m')
--   isempty <- isEmptyMVar m'
--   case isempty of
--     True -> print "pusto" >> pure [b] 
--     False ->fmap (b :) (readAllScope m')
-- --fmap (:) (pure b) readAllScope m'
--
-- chan :: IO ()
-- chan = do
--   gs <- newScope
--   global <- makeLocalScope gs ["global"]
--   local <- makeLocalScope global ["local"]
--   anotherlocal <- makeLocalScope global ["anotherlocal"]
--   local2 <- makeLocalScope local ["lambda"]
--   anotherlocal2 <- makeLocalScope anotherlocal ["anotherlabmda"]
--   value <- readAllScope local2
--   value2 <- readAllScope anotherlocal2
--   -- value <- readAllScope gs 
--   print "Value:"
--   print value
--   print "ANother Value:"
--   print value2
--   -- value <- readAllScope local
--   -- -- value <- readAllScope gs 
--   -- print "Value:"
--   -- print value
--   -- e1 <- newEnvironment
--   -- e2 <- addEnvironment e1 [2]
--   -- e3 <- addEnvironment e2 [3]
--   -- r1 <- readEnv e1
--   -- r2 <- readEnv e2
--   -- r3 <- readEnv e3
--
--   -- print (r1 :: ([Int]) )
--   -- print r2
--   -- print r3
--   print "Hello"
--  
--   -- print $ e2
--
