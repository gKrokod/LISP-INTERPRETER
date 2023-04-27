import Types 
import Parser

import Test.Hspec (context, describe, hspec, it, shouldBe, shouldNotBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (property)
import Text.Parsec
import Data.Either

main :: IO ()
main = hspec $ do
  describe "Bot logic" $ modifyMaxSuccess (const 1000) $ do
    context "Parser Number" $ do
      it "Input: Int" $ do
        property $ \int -> do
          fromRight (Number 0) (parse parseNumber "lisp" (show int))
            `shouldBe`  Number int 

      it "Input: Str" $ do
        property $ \str -> do
          let clearStr = filter (/='\"') str
          fromRight (String "") (parse parseString "lisp" (concat["\"", clearStr, "\""]) )
            `shouldBe` String clearStr 

      it "Input: Symbol" $ do
        property $ \str -> do
          let firstSymbol = "!$%&|*+-/:<=?>@^_~#" ++ ['A'..'Z'] ++ ['a'..'z'] 
          let restSymbol = firstSymbol ++ ['0'..'9']
          let str' = if null str then "a"
                     else if head str `elem` firstSymbol
                       then [head str] ++ filter (`elem` restSymbol) (tail str)
                       else "a"
          fromRight (Atom "") (parse parseAtom "lisp" str') 
            `shouldBe` Atom str' 
