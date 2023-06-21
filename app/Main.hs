-- todo
-- Eval + - * в самих себя. То есть (+) чтобы эвалился в +.? дискуссионно. samoopredelennue?
-- vozvedenie v stepen. ^ or **.
-- eval in, два аргумента, первй лямбда у которой берем окружение для вычисления второго аргумента в нем
-- проверить eval in не могу придумать пример
-- работу с числами с плаввающей запятой
-- работу со строками
-- лишние скобки ))))) в конце не рассматриваются парсером как ошибка
-- 
module Main (main) where
import Lisp.Types (Environment, SExpr(..))
-- import Parser
import Data.Char
import qualified Data.Map as Map
-- import Data.Foldable 
-- import qualified Handlers.Eval
import qualified Lisp.Handlers.Scope
import qualified Lisp.Handlers.Logger
import qualified Lisp.Handlers.Eval
import qualified Lisp.Eval.Eval
import qualified Lisp.Scope.Scope
-- import Scope.Scope (Binding)
-- import Parser (parseInput, clearComment)
import Lisp.Parser
import Text.Parsec (parse)
import qualified Data.Text.IO as TIO 
-- import Types (Binding)
import Control.Exception (SomeException, try, evaluate, PatternMatchFail)
type Tab = Int

main :: IO ()
main = do
-- make Scope
  nilScope <- Lisp.Scope.Scope.createEnvironment
  globalScope <- Lisp.Scope.Scope.makeLocalEnvironment nilScope (Map.fromList [("pi", Num 3.14)])
  let handleScope =
        Lisp.Handlers.Scope.Handle
          {   
            Lisp.Handlers.Scope.makeLocalEnvironment = Lisp.Scope.Scope.makeLocalEnvironment 
          , Lisp.Handlers.Scope.fillLocalEnvironment = Lisp.Scope.Scope.fillLocalEnvironment 
          , Lisp.Handlers.Scope.clearEnvironment = undefined 
          , Lisp.Handlers.Scope.check = Lisp.Scope.Scope.check
          , Lisp.Handlers.Scope.insert = Lisp.Scope.Scope.insert
          , Lisp.Handlers.Scope.update = Lisp.Scope.Scope.update
          }
-- set Logger
  let handleLog =
        Lisp.Handlers.Logger.Handle
          {   
            Lisp.Handlers.Logger.writeLog = \msg -> pure ()
            -- MyLisp.HHandlers.Logger.writeLog = \msg -> TIO.putStrLn $ "[LOG] " <> msg
          }

-- construct Eval
  let handleEval =
        Lisp.Handlers.Eval.Handle
          {   
            Lisp.Handlers.Eval.scope= handleScope
          , Lisp.Handlers.Eval.logger = handleLog
          , Lisp.Handlers.Eval.hRead = Lisp.Eval.Eval.hRead
          , Lisp.Handlers.Eval.hPrint = Lisp.Eval.Eval.hPrint
          }
  putStrLn "main end"  
-- start Interpretator
  loop handleEval globalScope
  -- loop handle secondScope

-- fi :: Handlers.Eval.Handle IO -> Environment -> String -> IO ()
-- fi h env x = 
--     case parse parseInput "lisp" (x) of
--       Left e -> print e
--       Right msg -> do
--         -- print msg 
--         resultEval <- Handlers.Eval.eval h env msg
--         print resultEval 

loop :: Lisp.Handlers.Eval.Handle IO -> Environment -> IO ()
loop h env = do
  putStr ">>> "
  input <- clearComment <$> getLine
  case input of 
    "test" -> do
      fileInput <- clearComment <$> readFile "Library/Test.lisp" 
      -- print fileInput
      -- let fInput = filter (not . null) $ lines $ clearComment fileInput
      -- mapM_ putStrLn fInput
      -- mapM_ (fi h env) fInput
      case parse parseInput "lisp" (fileInput) of
        Left e -> do
          putStrLn "error"
          print e
        Right msg -> do
          putStrLn "\n Parse Test file: \n"
          print msg 
          putStrLn "\n Eval: \n"
          resultEval <- try @PatternMatchFail $ evaluate (Lisp.Handlers.Eval.eval h env msg)
          -- resultEval <- try @PatternMatchFail (MyLisp.Handlers.Eval.eval h env msg)
          case resultEval of
              Left e -> do
                -- print e--(e :: PatternMatchFail)
                putStrLn "exception pattern \n"
              Right r1 -> do 
                result <- show <$> r1
                putStrLn "exception right"
                -- print r1
                putStrLn result
          -- print resultEval 
    "base" -> do
      fileInput <- clearComment <$> readFile "Library/Library.lisp" 
      case parse parseInput "lisp" (fileInput) of
        Left e -> do
          putStrLn "error Library file"
          print e
        Right msg -> do
          putStrLn "\n Parse Library file: \n"
          print msg 
          putStrLn "\n Eval: \n"
          resultEval <- Lisp.Handlers.Eval.eval h env msg
          print resultEval 
    otherwise -> do  
        case parse parseInput "lisp" input of
          Left e -> print e
          Right msg -> do
            -- print msg 
            resultEval <- try @SomeException $ evaluate (Lisp.Handlers.Eval.eval h env msg)
            case resultEval of
              Left e -> do
                putStrLn "I can't eval \n"
                -- print $ show e --(e :: PatternMatchFail)
                -- putStrLn "exception pattern \n"
                loop h env
              Right r -> do 
                putStrLn "exception right"
                result <- show <$> r
                putStrLn result 
        -- pure ()
  loop h env

--
