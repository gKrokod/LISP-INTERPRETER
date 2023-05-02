module Main (main) where
import Types
import Parser

import qualified Data.Map as Map
import Data.Foldable
-- import qualified Handlers.Eval
import qualified Handlers.Scope
import qualified Scope.Scope


main :: IO ()
main = do
  global <- Scope.Scope.createEnvironment
  firstScope <- Scope.Scope.makeLocalEnvironment global (Map.fromList [("pi", Number 3)])
  secondScope <- Scope.Scope.makeLocalEnvironment firstScope (Map.fromList [("xi", Number 10)])
  let handleScope =
        Handlers.Scope.Handle
          {   
            Handlers.Scope.makeLocalEnvironment = Scope.Scope.makeLocalEnvironment 
          , Handlers.Scope.clearEnvironment = undefined 
          }
  print "main end"
  -- loop handle secondScope


-- loop :: Handlers.Eval.Handle IO Binding -> Environment Binding -> IO ()
-- loop h env = do
--   msg <- readIOREPL
--   case msg of
--     Left e -> print e
--     Right p -> do 
--       -- newtok <- Handlers.Scope.evalSymbol h (Right p)
--       newtok <- Handlers.Eval.eval h env (Right p)
--       print "Result Eval: "
--       print newtok
--       print $ "Result Print:"
--       print p 
--       -- eEval <- evalIOREPL Map.empty (EvalToken p) 
--       -- case eEval of
--       --   Left e -> print e
--       --   Right (b, v) -> do
--       --     print b
--       --     print v --print ((evalREPL (EvalToken Map.empty p)) ) 
--   loop h env
--
