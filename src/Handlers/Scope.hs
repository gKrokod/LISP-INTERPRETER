module Handlers.Scope where
import Types
data Handle m a = Handle
  -- {   eval :: EvalToken -> m (EvalToken) 
    { environment :: Environment a
    , makeLocalEnvironment :: Environment a -> a -> m (Environment a)
  }

helloChan :: IO ()
helloChan = putStrLn "Hello CHan"

