module Handlers.Eval where
import qualified Handlers.Scope as S
import qualified Handlers.Logger as L
import qualified Handlers.Logger
import qualified Handlers.Scope
import Types
import Data.List (foldl1')
import qualified Data.Map as Map
import qualified Data.Text as T

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
eval h env expr@(List []) = do
  L.writeLog (logger h) "eval empty list" 
  pure expr
eval h env (Atom symbol) = do 
  L.writeLog (logger h) "eval Atom" 
  isSaved <- S.check (scope h) env symbol
  case isSaved of
    Nothing -> pure $ Atom symbol
    Just v -> pure v
--------------------------------------------------QUOTE
eval h env (List [Atom "quote", val]) = do
  L.writeLog (logger h) "eval quote" 
  pure val
 --                                         func SExpr
-- When func = SForm Lambda don't eval args and body.
eval h env (List [SForm LAMBDA , args , body]) = do
  L.writeLog (logger h) "eval lambda" 
  pure $ SForm $ LAMBDA' (atomExprToName args) body env
--------------------------------------------------DEF
eval h env (List [SForm DEF, Atom name, value]) = do
  L.writeLog (logger h) $ T.pack ("eval def " ++ show name ++ " " ++ show value)
  value' <- eval h env value -- vopros nado li vuchislat disskusionnuj
  S.insert (scope h) env name value'
  pure $ Bool True
--------------------------------------------------GET
eval h env (List [SForm GET, Atom name]) = do
  L.writeLog (logger h) $ T.pack ("eval get " ++ show name)
  isSaved <- S.check (scope h) env name
  case isSaved of
    Nothing -> pure $ Bool False
    Just v -> pure v
--------------------------------------------------SET
eval h env (List [SForm SET, Atom name, value]) = do
  L.writeLog (logger h) $ T.pack ("eval set " ++ show name ++ " " ++ show value)
  value' <- eval h env value -- vopros nado li vuchislat disskusionnuj
  S.update (scope h) env name value'
--------------------------------------------------LIST other
-- vuchislaen golovnyy formy do konza, esli ne spisok i ne atom, to evalit nechego mnogo raz
eval h env (List (func : args)) = do
  L.writeLog (logger h) $ T.pack ("eval list other " ++ show func)
  case func of
    List _ -> do
      func' <- eval h env func
      if func' == func then do
        L.writeLog (logger h) "LIst : func' == func " 
        apply h env func (mapM (eval h env) args)
      else do 
        L.writeLog (logger h) "LIst : func' /= func " 
        eval h env (List (func' : args))
    Atom _ -> do
      func' <- eval h env func
      if func' == func then do
        L.writeLog (logger h) "Atom: func' == func " 
        apply h env func (mapM (eval h env) args)
      else do 
        L.writeLog (logger h) "Atom: func' /= func " 
        eval h env (List (func' : args))
    otherwise -> apply h env func (mapM (eval h env) args)


apply :: (Monad m) => Handle m -> Environment -> SFunc -> m ([SExpr]) -> m SExpr
apply h env (BOper f) xs = do
  L.writeLog (logger h) "apply BOper func" 
  xs' <- xs
  case f of
    ADD -> pure $ foldl1' (\(Number x) (Number y) -> Number (x + y)) xs'
    SUB -> pure $ foldl1' (\(Number x) (Number y) -> Number (x - y)) xs'
    MUL -> pure $ foldl1' (\(Number x) (Number y) -> Number (x * y)) xs'
-- Add Gt, LT, EQ for another Types
apply h env (BPrim p) xs = do 
  L.writeLog (logger h) "apply BPrim func" 
  xs' <- xs
  case p of
    GT' -> pure $ foldl1' (\(Number x) (Number y) -> Bool (x > y)) xs'
    LT' -> pure $ foldl1' (\(Number x) (Number y) -> Bool (x < y)) xs'
    EQ' -> pure $ foldl1' (\(Number x) (Number y) -> Bool (x == y)) xs'

apply h env (SForm (LAMBDA' args body env')) xs = do
  L.writeLog (logger h) "lambda apply " 
  xs' <- xs
  newEnv <- S.makeLocalEnvironment (scope h) env' (Map.fromList $ zip args xs')
  eval h newEnv body
--Esli nichego ne nashli, verni poslednij resultat
apply h env func xs = do
  L.writeLog (logger h) "last chance apply, haven't func " 
  xs' <- xs
  if null xs' then pure func
              else pure $ last xs'
-- перевод из символов в именя для привязывания переменных в мапке 
atomExprToName :: SExpr -> [Name]
atomExprToName (Atom str) = [str]
atomExprToName (List xs) = concatMap atomExprToName xs

