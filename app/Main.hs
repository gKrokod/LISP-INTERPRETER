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
import Data.List (intercalate)

data Token = TInt Int | TDouble Double | TList [Token] | TStr String |
             TSymbol Char | TQuote Token | TPlus | TMul | TComment String deriving (Eq)

instance Show Token where
  show x = case x of
    TInt i -> show i
    TDouble d -> printf "%.2f" d 
    TQuote q -> "(quote " ++ show q ++ ")"
    TList [] -> "()"
    TList xs -> mconcat ["(", intercalate " " $ filter (not . null) $ map (\case {TComment _ -> ""; x -> show x}) xs, ")"]
    TStr s -> show s
    TSymbol c -> c : ""
    TPlus -> "+"
    TMul -> "*"
    TComment _ -> ""
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
-- char '"' *> (many $ noneOf "\"" ) >>= pure . TStr

parseTSymbol :: Parser Token
parseTSymbol = alphaNum  >>= pure . TSymbol

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


loop :: IO ()
loop = do
  putStr ">>> "
  inText <- getLine
  let isList = length (words inText) > 1
  let inText' = if isList then '(' : inText ++ ")" else inText
  case parse parseTToken "" inText' of
    Left e -> print e
    Right p -> print p 
  loop
