module Types where

import qualified Data.Map as Map
import Data.List (intercalate)
import Text.Printf
import Data.Char (toUpper)

data Token = TInt Int | TDouble Double | TList [Token] 
             | TStr String | TNil | TPil
             | TSymbol String | TQuote Token   -- убрать потом
             | TComment String | TEvalError String
             | BO BO | SF SF | BP BP deriving (Eq, Ord)

data BO = ADD | SUB | MUL | DIV | MOD | CONCAT deriving (Eq, Show, Ord)

data BP = GT' | LT' | EQ' deriving (Eq, Show, Ord)

data SF = DEF | SET | GET | QUOTE | TYPEOF | CONS | CAR 
          | CDR | COND | IF | PRINT | READ | EVAL 
          | EVALIN | LAMBDA | LAMBDA'
          | MACRO | MACROEXPAND | SYMBOL deriving (Eq, Show, Ord)


type Name = Token -- TSymbol example
type Value = Token -- TSymbol example
type EvalError = Token
type EvalToken = Either EvalError Value

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
    SF MACRO      -> "macro"
    SF MACROEXPAND-> "macroexpand" 
    SF SYMBOL-> "symbol" 
    TComment comment -> comment 
    _ -> error "token show"
