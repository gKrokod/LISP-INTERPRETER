-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DeriveAnyClass #-}
module Main (main) where

-- import Lib
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Printf
import Text.Parsec.Char
import Control.Monad (void)

data Token = TInt Int | TDouble Double | TList [Token] | TStr String | TSymbol Char | TQuote Token | TPlus | TMul deriving (Eq)
instance Show Token where
  show x = case x of
    TInt i -> show i
    TDouble d -> printf "%.2f" d 
    TQuote q -> "(quote " ++ show q ++ ")"
    TList [] -> "()"
    TList xs -> mconcat ["(", tail $ concatMap ((" " ++) . show) xs, ")"]
    TStr s -> show s
    TSymbol c -> c : ""
    TPlus -> "+"
    TMul -> "*"
    _ -> error "token"

d2 = TList []
d1 = TList [TInt 1, TPlus, TStr "haha", TMul]

main :: IO ()
main = do
  -- mapM_ print [b,c,d,e, DEF (\_ -> SYMBOL 'n')]
  loop

-- надо написать для каждого токена свой парсер, далее разбить свтроку по пробелам
-- и на каждый кусок натравить парсер по очереди, последним будет символ.
--
parseTInt :: Parser Token
parseTInt = do
  -- n <- many1 digit
  -- pure $ TInt $ read n 
  pure $ TInt 1

parseTToken :: Parser Token
parseTToken = lexeme $ choice [parseTList, parseTPlus, parseTMul, parseTStr, parseTQuote]
-- parseTToken = choice [parseTList, parseTPlus, parseTMul, parseTInt, parseTDouble, parseTStr, parseTQuote, parseTList]

parseTStr :: Parser Token
parseTStr = char '\"' *> (many $ noneOf "\"" ) >>= pure . TStr

parseTSymbol :: Parser Token
parseTSymbol = anyChar >>= pure . TSymbol 

parseTQuote :: Parser Token
-- parseTQuote = char '\'' *> choice [parseTPlus, parseTMul, parseTInt, parseTDouble, parseTStr, parseTQuote, parseTList, parseTSymbol] >>= pure . TQuote
parseTQuote = char '\'' *> choice [parseTPlus, parseTMul, parseTStr, parseTQuote, parseTList] >>= pure . TQuote

parseTPlus :: Parser Token
parseTPlus = char '+' *>  (pure $ TPlus) 

parseTMul ::  Parser Token
parseTMul = char '*' *>  (pure $ TMul) 

parseTList :: Parser Token 
parseTList = do
  void $ lexeme $ char '('
  -- e <- many $ lexeme $ choice [parseTPlus, parseTMul, parseTDouble, parseTInt,parseTStr, parseTQuote, parseTList, parseTSymbol]
  e <- many $ lexeme $ choice [parseTPlus, parseTMul, parseTStr, parseTQuote, parseTList]
  void $ lexeme $ char ')'
  return $ TList e
 
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"
--
--todo make for 1.0e24 form
--make for minus number
parseTDouble :: Parser Token
parseTDouble =parseTInt -- lexeme $ many1 digit >>= \h -> many digit >>= \t -> pure $ TDouble $ read (h ++ "." ++ t ++ "0")

-- parseTDouble = do 
--   h <- lexeme $ many1 digit
--   char '.'
--   t <- many digit 
--   pure $ TDouble $ read (h ++ "." ++ t ++ "0")
lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

loop :: IO ()
loop = do
  putStr ">>> "
  inText <- getLine
  case parse parseTToken "" inText of
    Left e -> print e
    Right p -> print p 
  loop
