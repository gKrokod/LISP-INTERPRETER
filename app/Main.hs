-- todo
-- Eval + - * в самих себя. То есть (+) чтобы эвалился в +.? дискуссионно
-- eval in, два аргумента, первй лямбда у которой берем окружение для вычисления второго аргумента в нем
-- проверить eval in не могу придумать пример
-- работу с числами с плаввающей запятой
-- работу со строками
-- больше или равно, меньше или равно
-- 
--  Сделать многострочный ввод
--  Сделать pretty printer
--  перенести в файл macro чистые функции
module Main (main) where
import Types (Environment, SExpr(..))
-- import Parser

import qualified Data.Map as Map
-- import Data.Foldable 
-- import qualified Handlers.Eval
import qualified Handlers.Scope
import qualified Handlers.Logger
import qualified Handlers.Eval
import qualified Eval.Eval
import qualified Scope.Scope
-- import Scope.Scope (Binding)
import Parser (parseInput, clearComment)
import Text.Parsec (parse)
import qualified Data.Text.IO as TIO 
-- import Types (Binding)
import Control.Exception (SomeException, try, evaluate, PatternMatchFail)

main :: IO ()
main = do
-- make Scope
  nilScope <- Scope.Scope.createEnvironment
  globalScope <- Scope.Scope.makeLocalEnvironment nilScope (Map.fromList [("pi", Number 3)])
  let handleScope =
        Handlers.Scope.Handle
          {   
            Handlers.Scope.makeLocalEnvironment = Scope.Scope.makeLocalEnvironment 
          , Handlers.Scope.fullLocalEnvironment = Scope.Scope.fullLocalEnvironment 
          , Handlers.Scope.clearEnvironment = undefined 
          , Handlers.Scope.check = Scope.Scope.check
          , Handlers.Scope.insert = Scope.Scope.insert
          , Handlers.Scope.update = Scope.Scope.update
          }
-- set Logger
  let handleLog =
        Handlers.Logger.Handle
          {   
            Handlers.Logger.writeLog = \msg -> pure ()
            -- Handlers.Logger.writeLog = \msg -> TIO.putStrLn $ "[LOG] " <> msg
          }

-- construct Eval
  let handleEval =
        Handlers.Eval.Handle
          {   
            Handlers.Eval.scope= handleScope
          , Handlers.Eval.logger = handleLog
          , Handlers.Eval.hRead = Eval.Eval.hRead
          , Handlers.Eval.hPrint = Eval.Eval.hPrint
          }
  putStrLn "main end"  
-- start Interpretator
  loop handleEval globalScope
  -- loop handle secondScope

fi :: Handlers.Eval.Handle IO -> Environment -> String -> IO ()
fi h env x = 
    case parse parseInput "lisp" (x) of
      Left e -> print e
      Right msg -> do
        -- print msg 
        resultEval <- Handlers.Eval.eval h env msg
        print resultEval 


loop :: Handlers.Eval.Handle IO -> Environment -> IO ()
loop h env = do
  putStr ">>> "
  input <- clearComment <$> getLine
  if input == "file" then do
    fileInput <- readFile "Library/Test.lisp"  
    -- print fileInput
    let fInput = filter (not . null) $ lines $ clearComment fileInput
    mapM_ putStrLn fInput
    mapM_ (fi h env) fInput
    -- case parse parseInput "lisp" (fInput) of
    --   Left e -> print e
    --   Right msg -> do
    --     -- print msg 
    --     resultEval <- Handlers.Eval.eval h env msg
    --     print resultEval 
  else  
    case parse parseInput "lisp" input of
      Left e -> print e
      Right msg -> do
        -- print msg 
       -- пишут, что надо обязательно evaluate писать, мол без него легко не словить ошибку, ну не знаю пока
        -- resultEval <- try @PatternMatchFail $ evaluate (Handlers.Eval.eval h env msg)
        resultEval <- try @PatternMatchFail (Handlers.Eval.eval h env msg)
        case resultEval of
          Left e -> do
            print e--(e :: PatternMatchFail)
            putStrLn "exception pattern \n"
            loop h env
          Right r1 -> do 
            putStrLn "exception right"
            print r1
      -- pure ()
  loop h env

--
