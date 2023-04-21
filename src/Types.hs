{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
module Types where

import qualified Data.Map as Map
import Data.List (intercalate)
import Text.Printf
import Data.Char (toUpper)
import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import qualified Data.Map as Map

data Token = TInt Int | TDouble Double | TList [Token] 
             | TStr String | TNil | TPil
             | TSymbol String | TQuote Token   -- убрать потом
             | TComment String | TEvalError String
             | BO BO | SF SF | BP BP deriving (Eq, Ord)

data BO = ADD | SUB | MUL | DIV | MOD | CONCAT deriving (Eq, Show, Ord)

data BP = GT' | LT' | EQ' deriving (Eq, Show, Ord)

data SF = DEF | SET | GET | QUOTE | TYPEOF | CONS | CAR 
          | CDR | COND | IF | PRINT | READ | EVAL 
          | EVALIN | LAMBDA | LAM Name Value EB | FUNCALL
          | MACRO | MACROEXPAND | SYMBOL deriving (Eq, Ord, Show )



type Name = Token -- TSymbol example
type Value = Token -- TSymbol example
type EvalError = Token
type EvalToken = Either EvalError Value
type EB = Environment Binding

-- newtype EB = MVar (Frame Binding) deriving Show

type Binding = Map.Map Name Value
-- окружение есть ящик содержащий фрейm
type Environment a = MVar (Frame a) 
-- фрейм из ящика окружения есть таблица связывания (Frame a)
-- и новый ящик и объемлющего окружения (enclosing environment) 
data Frame a = Frame a (Environment a)
-- Создаем родительской окружение, которое никуда не сылается, а содержит MVar ()
--
instance Show (Environment Binding) where
  show _ = "Environment Binding"
instance Ord (Environment Binding) where
 (<=) _ _ = True
-- instance Eq (Environment Binding) where
--  (==) _ _ = True
--  (==) _ _ = False

instance Show Token where
  show x = case x of
    TInt i -> show i
    TDouble d -> printf "%.2f" d 
    -- TQuote q -> "(tquote!!!! " ++ show q ++ ")"
    TList [] -> "()"
    TList xs -> mconcat ["(", intercalate " " $ map show xs, ")"]
    -- TList xs -> mconcat ["(", intercalate " " $ filter (not . null) $ map (\case {TComment _ -> ""; x -> show x}) xs, ")"]
    TStr s -> s
    TSymbol name -> map toUpper name
    TEvalError err -> err
    TNil -> "()"
    TPil -> "T"
    BO ADD -> "+"
    BO SUB -> "-"
    BO MUL -> "*"
    BO DIV -> "/"
    BO MOD -> "mod"
    BO CONCAT -> "++" 
    BP GT' -> ">"
    BP LT' -> "<"
    BP EQ' -> "=="
    SF DEF        -> "def"
    SF SET        -> "set!"
    SF GET        -> "get"
    SF QUOTE      -> "quote"
    SF TYPEOF     -> "typeof"
    SF CONS       -> "cons"
    SF CAR        -> "car"
    SF CDR        -> "cdr"
    SF COND       -> "cond"
    SF IF         -> "if"
    SF PRINT      -> "print"
    SF READ       -> "read"
    SF EVAL       -> "eval"
    SF EVALIN     -> "eval-in"
    SF LAMBDA     -> "lambda"
    SF (LAM a b c)  -> intercalate " " (["lam: ", show a, show b, show c])
    SF FUNCALL  -> "funcall" 
    SF MACRO      -> "macro"
    SF MACROEXPAND-> "macroexpand" 
    SF SYMBOL-> "symbol" 
    -- SF LAM (_) (_) (_) -> "LAM" 
    TComment comment -> comment 
    _ -> error "token show"
