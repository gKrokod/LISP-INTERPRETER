import Types 
import Parser

import Test.Hspec (context, describe, hspec, it, shouldBe, shouldNotBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (property)
import Text.Parsec
import Data.Either

main :: IO ()
main = hspec $ do
  describe "Parser" $ modifyMaxSuccess (const 1000) $ do
    context "Parser random atom" $ do
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
    it "fix test" $ do
      fromRight (Atom "") (parse parseAnySExpr "lisp" " '(  1  -2 3)") 
        `shouldBe` List [Atom "quote", List [Number 1, Number (-2), Number 3]] 
      fromRight (Atom "") (parse parseAnySExpr "lisp" "(      ()    ()    )") 
        `shouldBe` List [List [],List []] 
      fromRight (Atom "") (parse parseAnySExpr "lisp" " a b c d         e     ") 
        `shouldBe` Atom "e" 
      fromRight (Atom "") (parse parseAnySExpr "lisp" " 1 2 3 4         5     ") 
        `shouldBe` Number 5 
      fromRight (Atom "") (parse parseAnySExpr "lisp" "#t") 
        `shouldBe` (Bool True) 
      fromRight (Atom "") (parse parseAnySExpr "lisp" " #f") 
        `shouldBe` (Bool False) 
      fromRight (Atom "") (parse parseAnySExpr "lisp" "def") 
        `shouldBe` (SForm DEF) 
      fromRight (Atom "") (parse parseAnySExpr "lisp" "set!") 
        `shouldBe` (SForm SET) 
      fromRight (Atom "") (parse parseAnySExpr "lisp" "get") 
        `shouldBe` (SForm GET) 
      fromRight (Atom "") (parse parseAnySExpr "lisp" "    type-of") 
        `shouldBe` (SForm TYPEOF) 
      fromRight (Atom "") (parse parseAnySExpr "lisp" "cons") 
        `shouldBe` (SForm CONS) 
      fromRight (Atom "") (parse parseAnySExpr "lisp" "cdr") 
        `shouldBe` (SForm CDR) 
      fromRight (Atom "") (parse parseAnySExpr "lisp" "car") 
        `shouldBe` (SForm CAR) 
      fromRight (Atom "") (parse parseAnySExpr "lisp" "cond") 
        `shouldBe` (SForm COND) 
      fromRight (Atom "") (parse parseAnySExpr "lisp" "if") 
        `shouldBe` (SForm IF) 
      fromRight (Atom "") (parse parseAnySExpr "lisp" "read") 
        `shouldBe` (SForm READ) 
      fromRight (Atom "") (parse parseAnySExpr "lisp" "print") 
        `shouldBe` (SForm PRINT)
      fromRight (Atom "") (parse parseAnySExpr "lisp" "eval") 
        `shouldBe` (SForm EVAL)
      fromRight (Atom "") (parse parseAnySExpr "lisp" "eval-in") 
        `shouldBe` (SForm EVALIN)
      fromRight (Atom "") (parse parseAnySExpr "lisp" "l") 
        `shouldBe` (SForm LAMBDA)
      fromRight (Atom "") (parse parseAnySExpr "lisp" "macro") 
        `shouldBe` (SForm MACRO)
      fromRight (Atom "") (parse parseAnySExpr "lisp" "+") 
        `shouldBe` (BOper ADD)
      fromRight (Atom "net minusa") (parse parseAnySExpr "lisp" "-") 
        `shouldBe` (BOper SUB)
      fromRight (Atom "") (parse parseAnySExpr "lisp" "*") 
        `shouldBe` (BOper MUL)
      fromRight (Atom "") (parse parseAnySExpr "lisp" ">") 
        `shouldBe` (BPrim GT')
      fromRight (Atom "") (parse parseAnySExpr "lisp" "<") 
        `shouldBe` (BPrim LT')
      fromRight (Atom "") (parse parseAnySExpr "lisp" "==") 
        `shouldBe` (BPrim EQ')
