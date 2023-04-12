-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DeriveAnyClass #-}
module Main (main) where

-- import Lib
import Text.Parsec
import Text.Parsec.String (Parser)

data Token = TInt Int | TDouble Double | TList [Token] | TStr String | TSymbol Char | TPlus | TMul deriving (Eq)
instance Show Token where
  show x = case x of
    TInt i -> show i
    TList [] -> "()"
    TList xs -> mconcat ["(", tail $ concatMap ((" " ++) . show) xs, ")"]
    TStr s -> show s
    TSymbol c -> show c
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
  n <- many1 digit
  pure $ TInt $ read n 

loop :: IO ()
loop = do
  putStr ">>> "
  inText <- getLine
  putStrLn inText
  loop
