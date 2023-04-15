-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveFunctor #-}
module Main (main) where

import Parser (readIOREPL, readREPL)
import Types
import Control.Monad.State
import qualified Data.Map as Map
import Data.Foldable
import qualified Handlers.Scope
import qualified Scope
import qualified Eval.EvalFunction

  --   TList xs -> case (head xs) of
  --     BO ADD -> case sT (tail xs) of  --
  --       Left e -> pure $ (base , EvalToken e)
  --       Right v -> pure $ (base, EvalToken v)
  --     _ -> epure $ (base , EvalToken t)rror "Can't eval list"
  --   t -> pure $ (base , EvalToken t)

-- evalREPL :: EvalState -> EvalToken -> Either EvalError EvalDo
-- evalREPL base (EvalToken token) = do
--   case token of
--     TList xs -> case (head xs) of
--       BO func -> do
--         v <- funcBO func (tail xs)
--         pure $ (base, EvalToken v)
--       _ -> Left $ TEvalError "unknown func"
--     _ -> Right (base, EvalToken token)
-- -- Склоняюсь к мысли, что все же ЭВАЛ ДОЛЖЕН ДЕЛАТЬ ИЗ ЭВАЛА ЭВАЛ, ПОТОМУ ЧТО ИДЯ ПО СПИСКУ И СТАЛКИВАЯСЬ 
-- С НОВЫМИ ЭВАЛ надо же его вычислить, а не поулчить , короче надо эвалы думать, как переделать
-- советовал чел
-- eval :: Map Name Value -> Token -> IO Value 
-- CR
-- evalREPL :: EvalState -> EvalToken -> Either EvalError EvalDo
-- evalREPL base (EvalToken token) = do
--   case token of
--     TList xs -> case (head xs) of
--       BO func -> do
--         v <- funcBO func (tail xs)
--         pure $ (base, EvalToken v)
--       _ -> Left $ TEvalError "unknown func"
--     _ -> Right (base, EvalToken token)


-- можно передавать разные арифметические функции + - * /
--
-- funcBO  :: BO -> [Token] -> Either EvalError Token
-- funcBO ADD = funcBO' (+)
-- funcBO SUB = funcBO' (-)
-- funcBO MUL = funcBO' (*)
-- funcBO DIV = funcBO' (/)
-- funcBO MOD = funcBO' (\x y -> fromIntegral $ floor x `mod` floor y)
-- funcBO CONCAT = funcBO' (+)
--
-- funcBO'  :: (Double -> Double -> Double) -> [Token] -> Either EvalError Token
-- funcBO' g (xs) = (TDouble . foldl1 g) <$> mapM (\case
--                   TDouble x -> Right x
--                   TInt i -> Right $ fromIntegral i
--                   TList xs' -> do 
--                     (a, EvalToken b) <- evalREPL (Map.empty) (EvalToken $ TList xs')
--                     case b of
--                       -- TInt x -> Right $ fromIntegral x   -- почему-то работает, однако и без этой строки! что за гавноkk
--                       TDouble x -> Right $  x
--                       _ -> Left $ TEvalError $ "Can't eval these numbers' values"
--                   _ -> Left $ TEvalError $ "Can't eval these numbers' values") xs
--
--

-- loopRE :: EvalState -> EvalToken -> IO (EvalState, EvalToken)
-- loopRE base etoken = do
--    (b, t) <- evalREPL base etoken
--    loopRE b t

main :: IO ()
main = do
  scope <- Scope.newScope
  let handle =
        Handlers.Scope.Handle
          {   
              Handlers.Scope.writeLog = \log -> print $ "LOG: " <> log 
            , Handlers.Scope.check = Scope.check scope
            , Handlers.Scope.update = Scope.update scope
            , Handlers.Scope.funcBOMUL = Eval.EvalFunction.funcBOMUL
            , Handlers.Scope.funcBOADD = Eval.EvalFunction.funcBOADD
            , Handlers.Scope.funcBOSUB = Eval.EvalFunction.funcBOSUB
            , Handlers.Scope.funcBODIV = Eval.EvalFunction.funcBODIV
            , Handlers.Scope.funcBOMOD = Eval.EvalFunction.funcBOMOD
            , Handlers.Scope.funcBOCONCAT = Eval.EvalFunction.funcBOCONCAT
            , Handlers.Scope.funcBPGT = Eval.EvalFunction.funcBPGT
            , Handlers.Scope.funcBPLT = Eval.EvalFunction.funcBPLT
            , Handlers.Scope.funcBPEQ = Eval.EvalFunction.funcBPEQ
          }
  loop handle


loop :: Handlers.Scope.Handle IO -> IO ()
loop h = do
  msg <- readIOREPL
  case msg of
    Left e -> print e
    Right p -> do 
      -- newtok <- Handlers.Scope.evalSymbol h (Right p)
      newtok <- Handlers.Scope.eval h (Right p)
      print "Result Eval: "
      print newtok
      print $ "Result Print:"
      print p 
      -- eEval <- evalIOREPL Map.empty (EvalToken p) 
      -- case eEval of
      --   Left e -> print e
      --   Right (b, v) -> do
      --     print b
      --     print v --print ((evalREPL (EvalToken Map.empty p)) ) 
  loop h

