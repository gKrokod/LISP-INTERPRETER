import Types 
import Parser

import Test.Hspec (context, describe, hspec, it, shouldBe, shouldNotBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (property)
import Text.Parsec
import Data.Either
import Data.Bool
import qualified Handlers.Scope
import qualified Handlers.Logger
import qualified Handlers.Eval
import qualified Eval.Eval
import qualified Scope.Scope
import qualified Data.Map as Map

main :: IO ()
main = hspec $ do
  describe "Parser" $ modifyMaxSuccess (const 10) $ do
    context "Parser random atom" $ do
      it "Input: Int" $ do
        property $ \int -> do
          fromRight (Number 0) (parse parseNumber "lisp" (show int))
            `shouldBe`  Number int 

      it "Input: Double" $ do
        property $ \double -> do
          fromRight (Num 0.0) (parse parseNum "lisp" (show double))
            `shouldBe`  Num double

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
          let dictionary = ["<", ">", "==", "-","+","*", "lambda", "#t", "#f", "def", "^","macro"] -- and some other
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
      fromRight (Atom "") (parse parseInput "lisp" "set") 
        `shouldBe` (SForm SET) 
      fromRight (Atom "") (parse parseInput "lisp" "get") 
        `shouldBe` (SForm GET) 
      fromRight (Atom "") (parse parseInput "lisp" "    typeof") 
        `shouldBe` (SForm TYPEOF) 
      fromRight (Atom "") (parse parseInput "lisp" "cons") 
        `shouldBe` (SForm CONS) 
      fromRight (Atom "") (parse parseInput "lisp" "cdr") 
        `shouldBe` (SForm CDR) 
      fromRight (Atom "") (parse parseInput "lisp" "car") 
        `shouldBe` (SForm CAR) 
      fromRight (Atom "") (parse parseInput "lisp" "cond") 
        `shouldBe` (SForm COND) 
      -- fromRight (Atom "") (parse parseInput "lisp" "if") 
        -- `shouldBe` (SForm IF) 
      fromRight (Atom "") (parse parseInput "lisp" "read") 
        `shouldBe` (SForm READ) 
      fromRight (Atom "") (parse parseInput "lisp" "print") 
        `shouldBe` (SForm PRINT)
      fromRight (Atom "") (parse parseInput "lisp" "eval") 
        `shouldBe` (SForm EVAL)
      fromRight (Atom "") (parse parseInput "lisp" "eval-in") 
        `shouldBe` (SForm EVALIN)
      fromRight (Atom "") (parse parseInput "lisp" "lambda") 
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
      fromRight (Atom "") (parse parseInput "lisp" "^") 
        `shouldBe` (BOper EXPT)

  describe "Base Library" $ do
    -- resultEval <- (Handlers.Eval.eval handleEval globalScope msg)
    -- show (resultEval) `shouldBe` "5"
    let handleScope =
          Handlers.Scope.Handle
                  {   
                    Handlers.Scope.makeLocalEnvironment = Scope.Scope.makeLocalEnvironment 
                  , Handlers.Scope.fullLocalEnvironment = Scope.Scope.fullLocalEnvironment 
                  , Handlers.Scope.clearEnvironment = undefined 
                  , Handlers.Scope.check = Scope.Scope.check
                  , Handlers.Scope.insert = Scope.Scope.insert
                  , Handlers.Scope.update = Scope.Scope.update
                  } -- :: Handlers.Scope.Handle IO
        -- set Logger
    let handleLog = Handlers.Logger.Handle
                  {Handlers.Logger.writeLog = \msg -> pure ()}
        -- construct Eval
    let h =
                Handlers.Eval.Handle
                  {   
                    Handlers.Eval.scope= handleScope
                  , Handlers.Eval.logger = handleLog
                  , Handlers.Eval.hRead = undefined --Eval.Eval.hRead
                  , Handlers.Eval.hPrint = undefined --Eval.Eval.hPrint
                  }
    context "Car, cdr, family" $ do
      it "car, cdr, cdar, caar,cadr,cddr,cdddr,cadar,caddr,cadddr" $ do
              n <- Scope.Scope.createEnvironment
              env <- Scope.Scope.makeLocalEnvironment n (Map.empty)
              fileInput <- clearComment <$> readFile "Library/Library.lisp" 
              case parse parseInput "lisp" fileInput of
                Right msg -> do
                  resultEval <- Handlers.Eval.eval h env msg -- load base library
-- check answer
                  let test1 = parse' "car '((1 2)(3 4))"
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "(1 2)"

                  let test1 = parse' "car '(1 2 3 4)"
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "1"

                  let test1 = parse' "cdr '(1)"
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "NIL"

                  let test1 = parse' "cdr '(1 2)"
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "(2)"

                  let test1 = parse' "cdar '((1 2)(3 4))"
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "(2)"

                  let test1 = parse' "caar '((1 2)(3 4))"
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "1"

                  let test1 = parse' "cadr '((1 2)(3 4))"
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "(3 4)"

                  let test1 = parse' "cddr '((1 2)(3 4))"
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "NIL"

                  let test1 = parse' "cdddr '(1 2 3 4)"
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "(4)"

                  let test1 = parse' "cadar '((1 2)( 3 4) (5 6))"
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "2"

                  let test1 = parse' "caddr '((1 2)( 3 4) (5 6))"
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "(5 6)"

                  let test1 = parse' "cadddr '(1 2 3 4 5 6)"
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "4"
                _ -> undefined
    context "logic function" $ do
      it "and, or ,xor" $ do
              n <- Scope.Scope.createEnvironment
              env <- Scope.Scope.makeLocalEnvironment n (Map.empty)
              fileInput <- clearComment <$> readFile "Library/Library.lisp" 
              case parse parseInput "lisp" fileInput of
                Right msg -> do
                  resultEval <- Handlers.Eval.eval h env msg -- load base library
-- check answer
                  let test1 = parse' "and (> 2 1) (> 2 1)" -- t t = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#t"

                  let test1 = parse' "and (> 2 1) (< 2 1)" -- t f = f
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#f"

                  let test1 = parse' "and (< 2 1) (> 2 1)" -- f t = f
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#f"

                  let test1 = parse' "and (< 2 1) (< 2 1)" -- f f = f
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#f"

                  let test1 = parse' "or (> 2 1) (> 2 1)" -- t t = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#t"

                  let test1 = parse' "or (> 2 1) (< 2 1)" -- t f = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#t"

                  let test1 = parse' "or (< 2 1) (> 2 1)" -- f t = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#t"

                  let test1 = parse' "or (< 2 1) (< 2 1)" -- f f = f
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#f"

                  let test1 = parse' "xor (> 2 1) (> 2 1)" -- t t = f
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#f"

                  let test1 = parse' "xor (> 2 1) (< 2 1)" -- t f = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#t"

                  let test1 = parse' "xor (< 2 1) (> 2 1)" -- f t = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#t"

                  let test1 = parse' "xor (< 2 1) (< 2 1)" -- f f = f
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#f"

                  let test1 = parse' "not (> 2 1)" -- t = f
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#f"

                  let test1 = parse' "not (< 2 1)" -- f = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#t"

      it "greaterp, greqp, lessp, leeqp, eq, neq" $ do
              n <- Scope.Scope.createEnvironment
              env <- Scope.Scope.makeLocalEnvironment n (Map.empty)
              fileInput <- clearComment <$> readFile "Library/Library.lisp" 
              case parse parseInput "lisp" fileInput of
                Right msg -> do
                  resultEval <- Handlers.Eval.eval h env msg -- load base library
-- check answer
                  let test1 = parse' "(greaterp 2 1)" -- t t = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#t"

                  let test1 = parse' "(greaterp 1 2)" -- t t = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#f"

                  let test1 = parse' "(greqp 2 1)" -- t t = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#t"

                  let test1 = parse' "(greqp 2 2)" -- t t = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#t"

                  let test1 = parse' "(greqp 1 2)" -- t t = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#f"

                  let test1 = parse' "(lessp 2 1)" -- t t = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#f"

                  let test1 = parse' "(lessp 1 2)" -- t t = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#t"

                  let test1 = parse' "(leeqp 2 1)" -- t t = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#f"

                  let test1 = parse' "(leeqp 2 2)" -- t t = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#t"

                  let test1 = parse' "(leeqp 1 2)" -- t t = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#t"

                  let test1 = parse' "(eq 2 2)" -- t t = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#t"

                  let test1 = parse' "(eq 1 2)" -- t t = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#f"

                  let test1 = parse' "(neq 2 2)" -- t t = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#f"

                  let test1 = parse' "(neq 1 2)" -- t t = t
                  resultEval <- show <$> Handlers.Eval.eval h env test1
                  resultEval `shouldBe` "#t"

parse' :: String -> SExpr
parse' txt = case parse parseInput "lisp" txt of
  Right msg -> msg
  _ -> undefined 



