-- - {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveFunctor #-}
module Main (main) where

import Parser (readIOREPL, readREPL, sfRead)
import Types
import Control.Monad.State
import qualified Data.Map as Map
import Data.Foldable
import qualified Handlers.Eval
import qualified Handlers.Scope
import qualified Scope
import qualified Eval.EvalFunction


main :: IO ()
main = do
  global <- Scope.createEnvironment
  firstScope <- Scope.makeLocalEnvironment global (Map.fromList [(TSymbol "pi", TDouble 3.14)])
  secondScope <- Scope.makeLocalEnvironment firstScope (Map.fromList [(TSymbol "xi", TDouble 100003.14)])
  let handle =
        Handlers.Eval.Handle
          {   
              -- Handlers.Eval.writeLog = \log -> print $ "LOG: " <> log 
              Handlers.Eval.writeLog = \log -> pure ()
            , Handlers.Eval.check = Scope.check
            , Handlers.Eval.insert = Scope.insert
            , Handlers.Eval.update = Scope.update
            , Handlers.Eval.funcBOMUL = Eval.EvalFunction.funcBOMUL
            , Handlers.Eval.funcBOADD = Eval.EvalFunction.funcBOADD
            , Handlers.Eval.funcBOSUB = Eval.EvalFunction.funcBOSUB
            , Handlers.Eval.funcBODIV = Eval.EvalFunction.funcBODIV
            , Handlers.Eval.funcBOMOD = Eval.EvalFunction.funcBOMOD
            , Handlers.Eval.funcBOCONCAT = Eval.EvalFunction.funcBOCONCAT
            , Handlers.Eval.funcBPGT = Eval.EvalFunction.funcBPGT
            , Handlers.Eval.funcBPLT = Eval.EvalFunction.funcBPLT
            , Handlers.Eval.funcBPEQ = Eval.EvalFunction.funcBPEQ
            , Handlers.Eval.funcSFTYPEOF = Eval.EvalFunction.funcSFTYPEOF
            , Handlers.Eval.hPrint = putStrLn . Eval.EvalFunction.sfPrint
            , Handlers.Eval.hRead = sfRead
            , Handlers.Eval.makeLocalEnvironment' = Scope.makeLocalEnvironment
            -- , Handlers.Eval.environment = secondScope
            
          }
  loop handle secondScope


loop :: Handlers.Eval.Handle IO Binding -> Environment Binding -> IO ()
loop h env = do
  msg <- readIOREPL
  case msg of
    Left e -> print e
    Right p -> do 
      -- newtok <- Handlers.Scope.evalSymbol h (Right p)
      newtok <- Handlers.Eval.eval h env (Right p)
      print "Result Eval: "
      print newtok
      -- print $ "Result Print:"
      -- print p 
      -- eEval <- evalIOREPL Map.empty (EvalToken p) 
      -- case eEval of
      --   Left e -> print e
      --   Right (b, v) -> do
      --     print b
      --     print v --print ((evalREPL (EvalToken Map.empty p)) ) 
  loop h env

