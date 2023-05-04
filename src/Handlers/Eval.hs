module Handlers.Eval where
import qualified Handlers.Scope as S
import qualified Handlers.Logger as L
import qualified Handlers.Logger
import qualified Handlers.Scope
import Types
import Data.List (foldl1')
import qualified Data.Map as Map

type SFunc = SExpr -- SF, BO, BP

data Handle m = Handle {
    scope :: Handlers.Scope.Handle m
  , logger :: Handlers.Logger.Handle m
}
--elementary SExpr
eval :: (Monad m) => Handle m -> Environment -> SExpr -> m (SExpr)
eval h env expr@(Number _) = do
  L.writeLog (logger h) "eval Number" 
  pure expr
eval h env expr@(String _) = do 
  L.writeLog (logger h) "eval String" 
  pure expr
eval h env expr@(Bool _) = do 
  L.writeLog (logger h) "eval Bool" 
  pure expr
eval h env (List [Atom "quote", val]) = do
  L.writeLog (logger h) "eval quote" 
  pure val
eval h env (Atom symbol) = do 
  L.writeLog (logger h) "eval Atom" 
  isSaved <- S.check (scope h) env symbol
  case isSaved of
    Nothing -> pure $ Atom symbol
    Just v -> pure v
--func SExpr
eval h env (List [SForm LAMBDA , args , body]) = do
  L.writeLog (logger h) "eval lambda" 
  pure $ SForm $ LAMBDA' (atomExprToName args) body env
-- Если в голове список, то вычисли его и повтори
eval h env (List ( (List xs) : args)) = do
  L.writeLog (logger h) "eval list" 
  evalHead <- eval h env (List xs)
  eval h env (List (evalHead : args))
--
eval h env (List (func : args)) = do
  L.writeLog (logger h) "eval func" 
  apply h env func ( mapM (eval h env) args)

apply :: (Monad m) => Handle m -> Environment -> SFunc -> m ([SExpr]) -> m SExpr
apply h env (BOper ADD) xs = do
  L.writeLog (logger h) "Add apply func" 
  xs' <- xs
  pure $ foldl1' (\(Number x) (Number y) -> Number (x + y)) xs'

apply h env (SForm (LAMBDA' args body env')) xs = do
  L.writeLog (logger h) "lambda apply " 
  xs' <- xs
  newEnv <- S.makeLocalEnvironment (scope h) env' (Map.fromList $ zip args xs')
  eval h newEnv body

-- перевод из символов в именя для привязывания переменных в мапке 
atomExprToName :: SExpr -> [Name]
atomExprToName (Atom str) = [str]
atomExprToName (List xs) = concatMap atomExprToName xs

  -- value <- check h env token 
  -- case value of
  --   Right v -> do
  --      writeLog h $ ("exist in scope " ++ show v )
  --      pure $ Right v 
  --   Left _ -> case token of 
  --     (TEvalError e) -> pure $ Left $ TEvalError e
  --     _ -> funcEval h env token 
