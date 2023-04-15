module Handlers.Scope where
import Types
import Prelude hiding (check)
import Control.Monad
data Handle m = Handle
  -- {   eval :: EvalToken -> m (EvalToken) 
    { writeLog :: String -> m ()
    , check :: Name -> m (Either EvalError Value)
    , update :: Name -> Value -> m ()
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

evalList :: (Monad m) => Handle m -> EvalToken -> m (EvalToken)
evalList h (Left x) = do
  writeLog h $ "EvalError argument list" ++ show x 
  pure $ Left x
evalList h (Right (TList [])) = pure $ Right $ TNil
evalList h (Right (TList (x : xs))) =
  case x of
    BO ADD -> pure $ fadd' (Right $ head xs) (Right $ head $ tail xs)
      where fadd' :: EvalToken -> EvalToken -> EvalToken
            fadd' (Left a) _ = Left a
            fadd'  _ (Left b) = Left b
            fadd' (Right a) (Right b) = Right $ fadd a b
            fadd (TDouble x) (TDouble y) = TDouble (x + y)
            fadd (TInt x) (TInt y) = TInt (x + y)
            fadd (TDouble x) (TInt y) = TDouble (x + fromIntegral y)
            fadd (TInt x) (TDouble y) = TDouble (fromIntegral x + y)
            fadd (TStr s1) (TStr s2) = TStr (s1 ++ s2)
            -- fadd (TList xs') y = fadd (evalList h xs') y
            -- fadd x (TList ys') = fadd  x (evalList h (ys'))
            fadd _ _ = TEvalError "Wront List to eval"  
     -- where fadd = undefined
     --       g = undefined
                -- Left _ -> pure $ Left $ TEvalError ""
                -- Right _ -> pure $ Right $ 
      --           TEvalError s -> pure $ Left $ TEvalError s
      --           TDouble d -> pure $ Right $ TDouble d
      --           TInt i -> pure $ Right $ TInt i
      --           TStr s -> pure $ Right $ TStr s
      --           _ -> pure $ Left $ TEvalError "unknown list"
      -- where fadd :: Token -> Token -> Token
      --       fadd (TDouble x) (TDouble y) = TDouble (x + y)
      --       fadd (TInt x) (TInt y) = TInt (x + y)
      --       fadd (TDouble x) (TInt y) = TDouble (x + fromIntegral y)
      --       fadd (TInt x) (TDouble y) = TDouble (fromIntegral x + y)
      --       fadd (TStr s1) (TStr s2) = TStr (s1 ++ s2)
      --       fadd (TList xs') y = fadd (evalList h xs') y
      --       fadd x (TList ys') = fadd  x (evalList h (ys'))
      --       fadd _ _ = TEvalError "Wront List to eval"  
            
    BO SUB -> undefined
    BO MUL -> undefined
    BO DIV -> undefined
    BO MOD -> undefined
    BO CONCAT -> undefined
    BP GT' -> undefined
    BP LT' -> undefined
    BP EQ' -> undefined
    _ -> do
      writeLog h "This isn't eval list"
      pure $ Left $ TEvalError "This isn't eval list"
 
-- funcBO  :: BO -> [Token] -> Either EvalError Token
-- funcBO ADD = funcBO' (+)
-- funcBO SUB = funcBO' (-)
-- funcBO MUL = funcBO' (*)
-- funcBO DIV = funcBO' (/)
-- funcBO MOD = funcBO' (\x y -> fromIntegral $ floor x `mod` floor y)
-- funcBO CONCAT = funcBO' (+)
--
-- funcBO'  :: (Double -> Double -> Double) -> [Token] -> Either EvalError Token
-- funcBO' g (xs) = (TDouble . foldl1 g) <$> mapM (\case
--                   TDouble x -> Right x
--                   TInt i -> Right $ fromIntegral i
--                   TList xs' -> do 
--                     (a, EvalToken b) <- evalREPL (Map.empty) (EvalToken $ TList xs')
--                     case b of
--                       -- TInt x -> Right $ fromIntegral x   -- почему-то работает, однако и без этой строки! что за гавноkk
--                       TDouble x -> Right $  x
--                       _ -> Left $ TEvalError $ "Can't eval these numbers' values"
--                   _ -> Left $ TEvalError $ "Can't eval these numbers' values") xs
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
