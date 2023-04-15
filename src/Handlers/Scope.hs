module Handlers.Scope where
import Types
import Prelude hiding (check)
import Control.Monad
import Data.Typeable
import Data.List (foldl1')

data Handle m = Handle
  -- {   eval :: EvalToken -> m (EvalToken) 
    { writeLog :: String -> m ()
    , check :: Name -> m (Either EvalError Value)
    , update :: Name -> Value -> m ()
    , funcBOMUL :: Value -> Value -> Value
    , funcBOADD :: Value -> Value -> Value
    , funcBOSUB :: Value -> Value -> Value
    , funcBODIV :: Value -> Value -> Value
    , funcBOMOD :: Value -> Value -> Value
    , funcBOCONCAT :: Value -> Value -> Value
    , funcBPGT :: Value -> Value -> Value
    , funcBPLT :: Value -> Value -> Value
    , funcBPEQ :: Value -> Value -> Value
  }

-- type Name = Token -- TSymbol example
-- type Value = Token -- TSymbol example
-- type EvalError = Token TEvalError String
-- type EvalToken = Either EvalError Value

eval :: (Monad m) => Handle m -> EvalToken -> m (EvalToken)
eval h (Left x) = do
  writeLog h $ "EvalError argument" ++ show x 
  pure $ Left x
eval h (Right x) = 
  case x of  
    TSymbol _ -> evalSymbol h x 
    TStr _ -> evalStr h x
    TInt _ -> evalInt h x
    TDouble _ -> evalDouble h x
    TNil -> evalNil h x
    TPil -> evalPil h x
    TList xs -> evalList h (Right x)
    _ -> do
      writeLog h $ "This isn't Token"
      pure $ Right x 

funcOn :: (Token -> Token -> Token) -> EvalToken -> EvalToken -> EvalToken
funcOn f a b = do
  x <- a
  y <- b
  case f x y of
    TEvalError m -> Left $ TEvalError m
    token -> Right token

evalList :: (Monad m) => Handle m -> EvalToken -> m (EvalToken)
evalList h (Left x) = do
  writeLog h $ "EvalError argument list" ++ show x 
  pure $ Left x
evalList h (Right (TList [])) = pure $ Right $ TNil
evalList h (Right (TList (func : xs))) =
  case func of
    BO MUL -> do
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      -- writeLog h $ (show (typeOf xs'))
      pure $ foldl1' (funcOn (funcBOMUL h))  xs'
    BO ADD -> do
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      pure $ foldl1' (funcOn (funcBOADD h))  xs'
    BO SUB -> do
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      pure $ foldl1' (funcOn (funcBOSUB h))  xs'
    BO DIV -> do
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      pure $ foldl1' (funcOn (funcBODIV h))  xs'
    BO MOD -> do
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      pure $ foldl1' (funcOn (funcBOMOD h))  xs'
    BO CONCAT -> do
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      pure $ foldl1' (funcOn (funcBOCONCAT h))  xs'
    BP GT' -> do
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      pure $ foldl1' (funcOn (funcBPGT h)) xs'
    BP LT' -> do
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      pure $ foldl1' (funcOn (funcBPLT h)) xs'
    BP EQ' -> do
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      pure $ foldl1' (funcOn (funcBPEQ h)) xs'
    _ -> do
      writeLog h "This isn't eval list"
      pure $ Left $ TEvalError "This isn't eval list"
 
evalInt :: (Monad m) => Handle m -> Token -> m (EvalToken)
evalInt h (TInt i) = if i > 10000
  then do
    writeLog h $ "Very Big Int man!"
    pure $ Left $ TEvalError "Very Big Int man!"
  else pure $ Right $ TInt i

evalDouble :: (Monad m) => Handle m -> Token -> m (EvalToken)
evalDouble h double = pure $ Right $ double

evalNil :: (Monad m) => Handle m -> Token -> m (EvalToken)
evalNil h false = pure $ Right $ false
evalPil :: (Monad m) => Handle m -> Token -> m (EvalToken)
evalPil h true = pure $ Right $ true

evalSymbol :: (Monad m) => Handle m -> Token -> m (EvalToken)
evalSymbol h name = do
  value <- check h name
  case value of
    Left e -> do
	  writeLog h (show e)  
	  pure $ Right $ name 
    Right v -> do
       writeLog h $ ("exist in scope " ++ show v )
       pure $ Right v 
evalStr :: (Monad m) => Handle m -> Token -> m (EvalToken)
evalStr h string = pure $ Right $ string
-- evalSymbol :: (Monad m) => Handle m -> EvalToken -> m (EvalToken)
-- evalSymbol h (Left x) = do
--   writeLog h $ "EvalError argument " ++ show x 
--   pure $ Left x -- $ TEvalError "EvalError argument " 
-- evalSymbol h (Right (TSymbol name)) = do
--   value <- check h (TSymbol name)
--   case value of
--     Left e -> do
-- 	  writeLog h (show e)  
-- 	  pure $ Right $ TSymbol name 
--     Right v -> do
--        writeLog h $ ("exist in scope " ++ show v )
--        pure $ Right v 
