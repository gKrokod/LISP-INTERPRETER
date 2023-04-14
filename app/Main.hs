-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveFunctor #-}
module Main (main) where

import Parser (readREPL)
import Types
import  Control.Monad.State
import qualified Data.Map as Map
import Data.Foldable

type Name = Token -- TSymbol String
type Value = Token -- TInt, TDouble, TList, TStr, TSymbol, BP BP

newtype EvalToken = EvalToken {
                      eToken :: Token}
                    -- , base  :: Map.Map Name Value }
instance Show EvalToken where
  show = show . eToken

type EvalState = Map.Map Name Value
type EvalError = Token

evalREPL :: EvalToken -> State EvalState EvalToken
evalREPL (EvalToken t) = do
  case t of
    TList xs -> case (head xs) of
      BO ADD -> case sT (tail xs) of  --
        Left e -> pure $ EvalToken e
        Right v -> pure $ EvalToken v
      _ -> error "tlist fynct"
    t -> pure $ EvalToken t



-- sT :: [Token] -> Either EvalError Token
-- sT xs = foldr f (Right 0) xs
--   where f (TInt i) acc = TDouble $ pure (+ fromIntegral i) <*> acc
--         f (TDouble d) acc = pure (d +) <*> acc
--         f _ acc = Left $ TEvalError "Can't add these values"

-- можно передавать разные арифметические функции + - * /
sT :: [Token] -> Either EvalError Token
sT xs = foldrM f (TDouble 0) xs
  where f (TInt i) (TDouble acc) = Right $ TDouble (acc + fromIntegral i)
        f (TDouble d) (TDouble acc) = Right $ TDouble (acc + d) 
        f x acc = Left $ TEvalError "Can't add these values"


-- kkk
-- sumToken :: [Token] -> Either EvalError Token
-- sumToken [] = 0
-- sumToken (TInt i : xs) = fromIntegral i + sumToken xs 
-- sumToken (TDouble d : xs) = d + sumToken xs 
-- sumToken _ = Left $ TEvalError "Can't add these values"


main :: IO ()
main = do
  loop

-- loopIO :: EvalState -> Token ->  IO (EvalState, Token)
-- loopIO base g = do
--   pure (base, t) 

loop :: IO ()
loop = do
  putStr ">>> "
  inText <- getLine
  case readREPL inText of
    Left e -> print e
    Right p -> print (evalState (evalREPL (EvalToken p)) Map.empty ) 
  loop
-- instance Show Token where
--   show x = case x of
--     TInt i -> show i
--     TDouble d -> printf "%.2f" d 
--     TQuote q -> "(quote " ++ show q ++ ")"
--     TList [] -> "()"
--     TList xs -> mconcat ["(", intercalate " " $ filter (not . null) $ map (\case {TComment _ -> ""; x -> show x}) xs, ")"]
--     TStr s -> show s
--     TSymbol name -> name
--     BO ADD -> "+"
--     BO SUB -> "-"
--     BO MUL -> "*"
--     BO DIV -> "/"
--     BO MOD -> "mod"
--     BO CONCAT -> "++" 
--     BP GT' -> ">"
--     BP LT' -> "<"
--     BP EQ' -> "=="
--     SF DEF        -> "def"
--     SF SET        -> "set!"
--     SF GET        -> "get"
--     SF QUOTE      -> "quote"
--     SF TYPEOF     -> "typeof"
--     SF CONS       -> "cons"
--     SF CAR        -> "car"
--     SF CDR        -> "cdr"
--     SF COND       -> "cond"
--     SF PRINT      -> "print"
--     SF READ       -> "read"
--     SF EVAL       -> "eval"
--     SF EVALIN     -> "eval-in"
--     SF LAMBDA     -> "lambda"
--     SF MACRO      -> "macro"
--     SF MACROEXPAND-> "macroexpand" 
--     TComment _ -> ""
--     _ -> error "token show"
