module Types where

import qualified Data.Map as Map
import Data.List (intercalate)
import Text.Printf

data Token = TInt Int | TDouble Double | TList [Token] 
             | TStr String
             | TSymbol String | TQuote Token   -- убрать потом
             | TComment String | TEvalError String
             | BO BO | SF SF | BP BP deriving (Eq)

data BO = ADD | SUB | MUL | DIV | MOD | CONCAT deriving (Eq, Show)

data BP = GT' | LT' | EQ' deriving (Eq, Show)

data SF = DEF | SET | GET | QUOTE | TYPEOF | CONS | CAR 
          | CDR | COND | PRINT | READ | EVAL | EVALIN | LAMBDA
          | MACRO | MACROEXPAND | SYMBOL deriving (Eq, Show)


instance Show Token where
  show x = case x of
    TInt i -> show i
    TDouble d -> printf "%.2f" d 
    TQuote q -> "(quote " ++ show q ++ ")"
    TList [] -> "()"
    TList xs -> mconcat ["(", intercalate " " $ map show xs, ")"]
    -- TList xs -> mconcat ["(", intercalate " " $ filter (not . null) $ map (\case {TComment _ -> ""; x -> show x}) xs, ")"]
    TStr s -> show s
    TSymbol name -> name
    TEvalError err -> err
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
    SF PRINT      -> "print"
    SF READ       -> "read"
    SF EVAL       -> "eval"
    SF EVALIN     -> "eval-in"
    SF LAMBDA     -> "lambda"
    SF MACRO      -> "macro"
    SF MACROEXPAND-> "macroexpand" 
    TComment comment -> comment 
    _ -> error "token show"
