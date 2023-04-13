-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DeriveAnyClass #-}
module Main (main) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Printf
import Text.Parsec.Char
import Control.Monad 
import qualified Control.Monad.State as S
import Data.List (intercalate)
import qualified Data.Map as Map

data Token = TInt Int | TDouble Double | TList [Token] | TStr String |
             TSymbol String | TQuote Token | TPlus 
             | TMul | TComment String | BO BO | SF SF | BP BP deriving (Eq)

data BO = ADD | SUB | MUL | DIV | MOD | CONCAT deriving EQ
data BP = GT | LT | EQ
data SF = DEF | SET | GET | QUOTE | TYPEOF | CONS | CAR 
          | CDR | COND | PRINT | READ | EVAL | EVALIN | LAMBDA
          | MACRO | MACROEXPAND | SYMBOL

newtype EvalToken = EvalToken { token :: Token }
data EvalState = EvalState {
                   varMap :: Map.Map String Token 
                 , index :: Int 
                 }

-- evalREPL :: Token -> (EvalState, Token)
-- evalRepl = undefined

instance Show Token where
  show x = case x of
    TInt i -> show i
    TDouble d -> printf "%.2f" d 
    TQuote q -> "(quote " ++ show q ++ ")"
    TList [] -> "()"
    TList xs -> mconcat ["(", intercalate " " $ filter (not . null) $ map (\case {TComment _ -> ""; x -> show x}) xs, ")"]
    TStr s -> show s
    TSymbol name -> name
    TPlus -> "+"
    TMul -> "*"
    TComment _ -> ""
    _ -> error "token"

d2 = TList []
d1 = TList [TInt 1, TPlus, TStr "haha", TMul]

main :: IO ()
main = do
  loop

loopIO :: EvalState -> Token ->  IO (EvalState, Token)
loopIO base t = do
  pure (base, t) 

--
parseAnyToken :: Parser Token
parseAnyToken = choice [parseTComment, parseTPlus, parseTMul, parseTQuote, parseTNum, parseTList, parseTStr, parseTSymbol]

parseTInt :: Parser Token
parseTInt = do
  n <- many1 digit
  pure $ TInt $ read n 
  -- pure $ TInt 1

parseTToken :: Parser Token
parseTToken = lexeme $ parseAnyToken

parseTStr :: Parser Token
parseTStr = do
  void $ char '"'
  str <- many $ noneOf "\"" 
  void $ char '"'
  pure $ TStr str

parseTComment :: Parser Token
parseTComment = do
  void $ char ';'
  cmt <- many $ noneOf ";" 
  void $ char ';'
  pure $ TComment cmt

parseTSymbol :: Parser Token
parseTSymbol = many1 ( noneOf ("() \n\t\"';" ++ ['0'.. '9'])) >>= pure . TSymbol

parseTQuote :: Parser Token
parseTQuote = char '\'' *> parseAnyToken >>= pure . TQuote
--
parseTPlus :: Parser Token
parseTPlus = char '+' *>  (pure $ TPlus) 

parseTMul ::  Parser Token
parseTMul = char '*' *>  (pure $ TMul) 

parseTList :: Parser Token 
parseTList = do
  void $ lexeme $ char '('
  e <- many $ lexeme $ parseAnyToken
  void $ lexeme $ char ')'
  return $ TList e
 
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"
--
--todo make for 1.0e24 form
--make for minus number
parseTNum :: Parser Token
parseTNum = do 
  h <- many1 digit
  (char '.' *> many digit >>= \t -> pure $ TDouble $ read (h ++ "." ++ t ++ "0")) <|> (pure $ TInt $ read h)

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

readREPL :: String -> Either ParseError Token
readREPL text = parse parseTToken "" text'
  where 
   txt = dropWhile (==' ') text
   isList = (not . null) txt && length (words txt) > 1 && head txt /= '('
   text' = if isList then '(' : txt ++ ")" else txt

loop :: IO ()
loop = do
  putStr ">>> "
  inText <- getLine
  case readREPL inText of
    Left e -> print e
    Right p -> print p 
  loop
