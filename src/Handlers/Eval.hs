module Handlers.Eval where
import qualified Handlers.Scope as S
import qualified Handlers.Logger as L
import qualified Handlers.Logger
import qualified Handlers.Scope
import Types
import Data.List (foldl1')
import qualified Data.Map as Map
import qualified Data.Text as T
import Eval.Macros (mExpand, atomExprToMacroName)
import Eval.Eval (atomExprToName, bprim, boper)
import Data.Function
import Control.Exception (SomeException, try, evaluate)

type SFunc = SExpr -- SF, BO, BP

data Handle m = Handle {
    scope :: Handlers.Scope.Handle m
  , logger :: Handlers.Logger.Handle m
  , hPrint :: SExpr -> m ()
  , hRead :: m (SExpr)
}
--elementary SExpr
-- todo сделать вычисления BP and BO форм в самих себя, т.е. (+) вычислялся, чтобы в +
-- или падать?
-- MonadFail for pattern matching as (x : _ ) <- xs
eval :: (MonadFail m, Monad m) => Handle m -> Environment -> SExpr -> m (SExpr)
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
--------------------------------------------------READ
-- !!!exist apply h env (SForm READ) xs = do    for eval express : (read)
eval h env (SForm READ) = do 
  L.writeLog (logger h) "eval Read" 
  hRead h
--------------------------------------------------QUOTE
eval h env (List [Atom "quote", val]) = do
  L.writeLog (logger h) "eval quote" 
  pure val
-- ------------------------------------------ LAMBDA
-- версия первая, когда окружение не создавали
eval h env (List [SForm LAMBDA, args , body]) = do
  L.writeLog (logger h) "eval lambda and don't make env" 
  pure $ SForm $ LAMBDA' (atomExprToName args) body env
-- весрия для эвали Ин, когда сразу создается окружение.
-- eval h env (List [SForm LAMBDA , args , body]) = do
--   L.writeLog (logger h) "eval lambda and make env" 
--   newEnv <- S.makeLocalEnvironment (scope h) env (Map.empty)
--   pure $ SForm $ LAMBDA' (atomExprToName args) body newEnv
----------------------------------------------------------MACRO
eval h env (List [SForm MACRO, args , body]) = do
  L.writeLog (logger h) "eval macro" 
  pure $ SForm $ MACRO' (atomExprToMacroName args) body 
--------------------------------------------------DEF
eval h env (List [SForm DEF, Atom name, value]) = do
  L.writeLog (logger h) $ T.pack ("eval def " ++ show name ++ " " ++ show value)
  value' <- eval h env value -- vopros nado li vuchislat disskusionnuj
  S.insert (scope h) env name value'
  -- pure $ Bool True
  pure $ Void
--------------------------------------------------GET
eval h env (List [SForm GET, Atom name]) = do
  L.writeLog (logger h) $ T.pack ("eval get " ++ show name)
  isSaved <- S.check (scope h) env name
  case isSaved of
    Nothing -> pure $ Bool False
    Just v -> pure v
--------------------------------------------------COND
eval h env (List (SForm COND : cond1 : other)) = do
  L.writeLog (logger h) $ T.pack ("eval cond " ++ show cond1)
  case cond1 of
    List [p, r] -> do
      p' <- eval h env p
      if p' == Bool True then eval h env r
      else eval h env (List (SForm COND : other))
    -- _ -> undefined
--------------------------------------------------SET
eval h env (List [SForm SET, name, value]) = do
-- eval h env (List [SForm SET, Atom name, value]) = do
  L.writeLog (logger h) $ T.pack ("eval set " ++ show name ++ " " ++ show value)
  value' <- eval h env value -- 
  name'  <- eval h env name 
  case name' of
    Atom name'' -> do
      -- L.writeLog (logger h) $ T.pack ("set atom name'")
      S.update (scope h) env name'' value'
    _ -> error "not atom"
-- set вычисляет оба аргумента, setq не вычисляет первый аргумент, csetq создает константу
-- возможно константу надо сохранять в отдельной области памяти. 
  -- S.update (scope h) env name' value'
-- --------------------------------------------------EVAL IN -- WARNING. возможно работает неправильно
eval h env (List (SForm EVALIN : lambdaKey : args)) = do
  L.writeLog (logger h) $ T.pack ("eval in II variant")
  lambda' <- eval h env lambdaKey
  case lambda' of
    SForm (LAMBDA' _ _ env') -> do
      L.writeLog (logger h) $ "GOT IT LAMBDA'" 
      eval h env' (List (SForm EVAL : args)) 
    _ -> error "evalin form"
--------------------------------------------------LIST other
-- вычисляем головную форму много раз для атома и списка. Если попадается макрос, то раскрываем его.
eval h env (List (func : args)) = do
  L.writeLog (logger h) $ T.pack ("eval list other " ++ show func)
  case func of
    List _ -> do
      func' <- eval h env func
      if func' == func then do
        -- L.writeLog (logger h) "LIst : func' == func " 
        apply h env func (mapM (eval h env) args)
      else do 
        -- L.writeLog (logger h) "LIst : func' /= func " 
        eval h env (List (func' : args))
    Atom _ -> do
      func' <- eval h env func
      if func' == func then do
        -- L.writeLog (logger h) "Atom: func' == func " 
        apply h env func (mapM (eval h env) args)
      else do 
        -- L.writeLog (logger h) "Atom: func' /= func " 
        eval h env (List (func' : args))
    (SForm (MACRO' macroArgs body)) -> do
      L.writeLog (logger h) "eval MACRO'"
      -- L.writeLog (logger h) $ T.pack $ "macroArgs: " ++ show macroArgs 
      -- L.writeLog (logger h) $ T.pack $ "vveli v macros args: " ++ show args 
      let macroTable = Map.fromList $ zip macroArgs args
      -- L.writeLog (logger h) $ T.pack $ "macroTable: " ++ show macroTable
      let body' = mExpand macroTable body
      L.writeLog (logger h) $ T.pack $ "MACROEXPAND: " ++ show body'
      eval h env body'
    otherwise -> apply h env func (mapM (eval h env) args)


apply :: (MonadFail m, Monad m) => Handle m -> Environment -> SFunc -> m ([SExpr]) -> m SExpr
apply h env (BOper f) xs = do
  L.writeLog (logger h) "apply BOper func. If empty list = error" 
  xs' <- xs
  case f of
    ADD -> pure $ foldl1' (boper (+)) xs'
    SUB -> pure $ foldl1' (boper (-)) xs'
    MUL -> pure $ foldl1' (boper (*)) xs' 
--
apply h env (BPrim p) xs = do 
  L.writeLog (logger h) "apply BPrim func.  If empty list = error" 
  (x : y : _) <- xs -- possible error pattern matching
  case p of
    GT' -> pure $ Bool $ bprim (>) x y
    GTQ' -> pure $ Bool $ bprim (>=) x y
    LT' -> pure $ Bool $ bprim (<) x y
    LTQ' -> pure $ Bool $ bprim (<=) x y
    EQ' -> pure $ Bool $ bprim (==) x y

apply h env (SForm TYPEOF) xs = do 
  L.writeLog (logger h) "apply type-of func. If empty list = error " 
  (x : _)  <- xs
  case x of
    Atom _ -> pure $ String "Atom"
    List _ -> pure $ String "List"
    Number _ -> pure $ String "Number"
    String _ -> pure $ String "String"
    Bool _ -> pure $ String "Bool"
    SForm _ -> pure $ String "SForm"
    BOper _ -> pure $ String "BOper"
    BPrim _ -> pure $ String "BPrim"

apply h env (SForm CAR) xs = do 
  L.writeLog (logger h) "apply car func. If empty list = error " 
  (x : _)  <- xs -- possible error pattern matching
  case x of
    List ( result : _) -> pure result
    -- _ -> undefined
  
apply h env (SForm CDR) xs = do 
  L.writeLog (logger h) "apply cdr func. If empty list = error " 
  (x : _)  <- xs -- possible error pattern matching
  case x of
    List ( _ : result) -> pure $ List result
    -- _ -> undefined

apply h env (SForm READ) xs = do 
  L.writeLog (logger h) "apply read func." 
  hRead h
  
apply h env (SForm PRINT) xs = do 
  L.writeLog (logger h) "apply print func. If empty list = error " 
  (x : _)  <- xs -- possible error pattern matching
  -- x <- head <$> xs
  hPrint h x
  -- pure $ Bool True
  pure $ Void

apply h env (SForm LIST) xs = do 
  L.writeLog (logger h) "apply print list. If empty list = error " 
  xs' <- xs
  pure $ List xs'

apply h env (SForm CONS) xs = do 
  L.writeLog (logger h) "apply cons func. error with no List arg " 
  xs' <- xs
  case xs' of
    [a, (List as)] -> pure $ List (a : as)
    -- _ -> undefined
    
apply h env (SForm EVAL) xs = do 
  L.writeLog (logger h) "apply eval func. error with no List arg " 
  (x : _)  <- xs -- possible error pattern matching
  -- x <- head <$> xs
  eval h env x
-------------------------------------------------- LABMDA' Две верси
-- версия первая
apply h env (SForm (LAMBDA' args body env')) xs = do
  L.writeLog (logger h) "lambda apply and make env " 
  xs' <- xs
  newEnv <- S.makeLocalEnvironment (scope h) env' (Map.fromList $ zip args xs')
  eval h newEnv body
-- версия для Эвал ин , когда окружение создается при отсутсвии фактических параметров
-- apply h env (SForm (LAMBDA' args body env')) xs = do
--   L.writeLog (logger h) "lambda apply and don't make env" 
--   xs' <- xs
--   -- mapM_ (\(name, sexpr) -> S.insert (scope h) env' name sexpr) (zip args xs')
--   S.fullLocalEnvironment (scope h) env' (Map.fromList $ zip args xs')
--   eval h env' body
--Esli nichego ne nashli, verni poslednij resultat
--
apply h env func xs = do
  L.writeLog (logger h) "last chance apply, haven't func " 
  xs' <- xs
  if null xs' then pure func
              else pure $ last xs'

