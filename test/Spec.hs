import Types 
import Parser
import Eval

import Test.Hspec (context, describe, hspec, it, shouldBe, shouldNotBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (property)
import Text.Parsec
import Data.Either
import Data.Bool

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
          let dictionary = ["<", ">", "==", "-","+","*", "l", "#t", "#f", "def"] -- and some other
          let str'' = bool str' (str' ++ "a") $ str' `elem` dictionary
          fromRight (Atom "") (parse parseAtom "lisp" str'') 
            `shouldBe` Atom str'' 

      it "Input: Parser any string = Parser (print any string) " $ do
        property $ \input1 -> do
          case parse parseInput "lisp" input1 of
            Left _ -> "LEFT" `shouldBe` "LEFT"
            Right txt -> Right txt `shouldBe` (parse parseInput "lisp" (show txt))

    it "fix test" $ do
      fromRight (Atom "") (parse parseInput "lisp" " '(  1  -2 3)") 
        `shouldBe` List [Atom "quote", List [Number 1, Number (-2), Number 3]] 
      fromRight (Atom "") (parse parseInput "lisp" (clearComment ";New code; '(  1  -2 3) ; This is a commentary;"))
        `shouldBe` List [Atom "quote", List [Number 1, Number (-2), Number 3]] 
      fromRight (Atom "") (parse parseInput "lisp" "(      ()    ()    )") 
        `shouldBe` List [List [],List []] 
      fromRight (Atom "") (parse parseInput "lisp" " a b c d         e     ") 
        `shouldBe` List [Atom "a", Atom "b", Atom "c", Atom "d", Atom "e"] 
      fromRight (Atom "") (parse parseInput "lisp" " 1 2 3 4         5     ") 
        `shouldBe` List [Number 1, Number 2, Number 3, Number 4, Number 5] 
      fromRight (Atom "") (parse parseInput "lisp" "#t") 
        `shouldBe` (Bool True) 
      fromRight (Atom "") (parse parseInput "lisp" " #f") 
        `shouldBe` (Bool False) 
      fromRight (Atom "") (parse parseInput "lisp" "def") 
        `shouldBe` (SForm DEF) 
      fromRight (Atom "") (parse parseInput "lisp" "set!") 
        `shouldBe` (SForm SET) 
      fromRight (Atom "") (parse parseInput "lisp" "get") 
        `shouldBe` (SForm GET) 
      fromRight (Atom "") (parse parseInput "lisp" "    type-of") 
        `shouldBe` (SForm TYPEOF) 
      fromRight (Atom "") (parse parseInput "lisp" "cons") 
        `shouldBe` (SForm CONS) 
      fromRight (Atom "") (parse parseInput "lisp" "cdr") 
        `shouldBe` (SForm CDR) 
      fromRight (Atom "") (parse parseInput "lisp" "car") 
        `shouldBe` (SForm CAR) 
      fromRight (Atom "") (parse parseInput "lisp" "cond") 
        `shouldBe` (SForm COND) 
      fromRight (Atom "") (parse parseInput "lisp" "if") 
        `shouldBe` (SForm IF) 
      fromRight (Atom "") (parse parseInput "lisp" "read") 
        `shouldBe` (SForm READ) 
      fromRight (Atom "") (parse parseInput "lisp" "print") 
        `shouldBe` (SForm PRINT)
      fromRight (Atom "") (parse parseInput "lisp" "eval") 
        `shouldBe` (SForm EVAL)
      fromRight (Atom "") (parse parseInput "lisp" "eval-in") 
        `shouldBe` (SForm EVALIN)
      fromRight (Atom "") (parse parseInput "lisp" "l") 
        `shouldBe` (SForm LAMBDA)
      fromRight (Atom "") (parse parseInput "lisp" "macro") 
        `shouldBe` (SForm MACRO)
      fromRight (Atom "") (parse parseInput "lisp" "+") 
        `shouldBe` (BOper ADD)
      fromRight (Atom "net minusa") (parse parseInput "lisp" "-") 
        `shouldBe` (BOper SUB)
      fromRight (Atom "") (parse parseInput "lisp" "*") 
        `shouldBe` (BOper MUL)
      fromRight (Atom "") (parse parseInput "lisp" ">") 
        `shouldBe` (BPrim GT')
      fromRight (Atom "") (parse parseInput "lisp" "<") 
        `shouldBe` (BPrim LT')
      fromRight (Atom "") (parse parseInput "lisp" "==") 
        `shouldBe` (BPrim EQ')

  describe "Eval" $ modifyMaxSuccess (const 1000) $ do
    context "random input" $ do
      it "Input: Number" $ do
        property $ \int -> do
          (eval $ Number int) 
            `shouldBe`  Number int 

      it "Input: Str" $ do
        property $ \str -> do
          let clearStr = filter (/='\"') str
          (eval $ String clearStr)
            `shouldBe` String clearStr 

    it "fix test" $ do
      (eval $ Bool True)
        `shouldBe` (Bool True )
      (eval $ Bool False)
        `shouldBe` (Bool False )
      (eval $ List [Atom "quote", Bool False])
        `shouldBe` ( Bool False)
      (eval $ List [Atom "quote", List [Number 1, Number 2]])
        `shouldBe` (List [Number 1, Number 2])

