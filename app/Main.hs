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
import Types
import Parser

import qualified Data.Map as Map
import Data.Foldable
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
import Types (Binding)


main :: IO ()
main = do
-- make Scope
  nilScope <- Scope.Scope.createEnvironment
  globalScope <- Scope.Scope.makeLocalEnvironment nilScope (Map.fromList [("pi", Number 3)])
  let handleScope =
        Handlers.Scope.Handle
          {   
            Handlers.Scope.makeLocalEnvironment = Scope.Scope.makeLocalEnvironment 
          , Handlers.Scope.clearEnvironment = undefined 
          , Handlers.Scope.check = Scope.Scope.check
          , Handlers.Scope.insert = Scope.Scope.insert
          , Handlers.Scope.update = Scope.Scope.update
          }
-- set Logger
  let handleLog =
        Handlers.Logger.Handle
          {   
            Handlers.Logger.writeLog = \msg -> TIO.putStrLn $ "[LOG] " <> msg
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
  print "main end"
-- start Interpretator
  loop handleEval globalScope
  -- loop handle secondScope

prettyPrinter ::

loop :: Handlers.Eval.Handle IO -> Environment -> IO ()
loop h env = do
  putStr ">>> "
  input <- clearComment <$> getLine
  case parse parseInput "lisp" input of
    Left e -> print e
    Right msg -> do
      -- print msg 
      resultEval <- Handlers.Eval.eval h env msg
      print resultEval 

      -- pure ()
  loop h env

--
