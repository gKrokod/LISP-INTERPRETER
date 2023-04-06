-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DeriveAnyClass #-}
module Main (main) where

-- import Lib

instance Show LEnum where
  show (DEF _) = show "func LEnum -> LEnum"
  show (OPER operation) = show operation
  show (LIST consList) = show consList
  show (STR string) = show string
  show (SYMBOL char) = show char

data LEnum = OPER Operation | LIST ConsList | STR String | SYMBOL Char | DEF (LEnum -> LEnum)

data Operation = Minus | Plus deriving Show
data ItemGList = N Int | S String | C Char deriving Show
type ConsList = [ItemGList]

gList :: ConsList 
gList = [N 14, S "hello", C 's']

b,c,d,e :: LEnum 
b = OPER Minus
c = LIST $ reverse gList 
d = STR "ya stroka"
e = SYMBOL 'c'


main :: IO ()
main = mapM_ print [b,c,d,e, DEF (\_ -> SYMBOL 'n')]
