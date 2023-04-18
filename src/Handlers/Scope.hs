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
    , funcSFTYPEOF :: Value -> Value
    , hPrint :: EvalToken -> m ()
    , hRead :: m (EvalToken)
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
    TStr _ -> withEval h evalStr x
    TInt _ -> withEval h evalInt x
    TDouble _ -> withEval h  evalDouble x
    TNil -> withEval h evalNil x
    TPil -> withEval h evalPil x
    TSymbol _ -> withEval h evalSymbol x 
    TList xs ->  withEvalList h evalList x--do
    other -> do
      -- writeLog h $ "This isn't Token for eval       " ++ show (other == BO ADD)
      pure $ Right x 

evalQuote :: (Monad m) => Token -> m (EvalToken)
evalQuote (TEvalError e) = pure $ Left $ TEvalError e
evalQuote token = pure $ Right token


withEval :: (Monad m) => Handle m -> (Handle m -> Token -> m (EvalToken)) -> Token -> m (EvalToken)
withEval h funcEval token = do
  value <- check h token 
  case value of
    Right v -> do
       writeLog h $ ("exist in scope " ++ show v )
       pure $ Right v 
    Left _ -> case token of 
      (TEvalError e) -> pure $ Left $ TEvalError e
      _ -> funcEval h token 


evalInt :: (Monad m) => Handle m -> Token -> m (EvalToken)
evalInt h (TInt i) = 
	  if i > 10000
	  then do
		writeLog h $ "Very Big Int man!"
		pure $ Left $ TEvalError "Very Big Int man!"
	  else pure $ Right $ TInt i


evalDouble :: (Monad m) => Handle m -> Token -> m (EvalToken)
evalDouble h double = pure $ Right $ double


withEvalList :: (Monad m) => Handle m -> (Handle m -> EvalToken -> m (EvalToken)) -> Token -> m (EvalToken)
withEvalList h funcEval token = do
  value <- check h token 
  case value of
    Right v -> do
       writeLog h $ ("exist List in scope " ++ show v )
       pure $ Right v
    Left _ -> case token of 
      (TEvalError e) -> pure $ Left $ TEvalError e
      _ -> funcEval h (Right token)

evalList :: (Monad m) => Handle m -> EvalToken -> m (EvalToken)
-- evalList h (Left x) = do
--   writeLog h $ "EvalError argument list" ++ show x 
--   pure $ Left x
evalList h (Right (TList [])) = pure $ Right $ TNil
evalList h (Right (TList (func : xs))) =
  case func of
    BO MUL -> do
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      -- writeLog h $ (show (typeOf xs'))
      -- menya pure na EVAL i OBRATNO. ny lan
      -- eval h $ foldl1' (funcOn (funcBOMUL h))  xs'
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
    SF TYPEOF -> do
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      case xs' of
        [x] -> do
          pure $ funcDuo (funcSFTYPEOF h) x
        otherwise -> do
          writeLog h "This isn't eval list for typeof"
          pure $ Left $ TEvalError "This isn't eval list"
    SF DEF -> do
      xs' <- mapM (eval h) (map Right (tail xs)) -- :: [EvalToken]
      case xs' of
        [Right value] -> do
          update h (head xs) value
          writeLog h (show (head xs) ++ " UPDATE TO " ++ show value )
          pure $ Right $ TNil
        _ -> do
          writeLog h $ "This isn't eval list for def" ++ show xs'
          pure $ Left $ TEvalError "This isn't eval list for def"
    SF SET -> do
      xs' <- mapM (eval h) (map Right (tail xs)) -- :: [EvalToken]
      case xs' of
        [Right newValue] -> do
          isExist <- check h (head xs) 
          case isExist of
            Left e -> do
              writeLog h ("This " ++ show (head xs) ++ " does not exitst in scope")
              pure $ Left $ TEvalError ("This " ++ show (head xs) ++ " does not exitst in scope")
            Right _ -> do 
              update h (head xs) newValue
              writeLog h (show (head xs) ++ " UPDATE TO " ++ show newValue )
              pure $ Right $ TNil
    SF GET -> do
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      case xs' of
        [Right name] -> do
          isExist <- check h (head xs) 
          case isExist of
            Left e -> do
              writeLog h ("This " ++ show (head xs) ++ " does not exitst in scope")
              pure $ Left $ TEvalError ("This " ++ show (head xs) ++ " does not exitst in scope")
            Right value -> do 
              writeLog h (show name ++ " fromScope " ++ show value) 
              pure $ Right $ value 
        _ -> do
          writeLog h "This isn't eval list for get"
          pure $ Left $ TEvalError "This isn't eval list for get"
    SF QUOTE -> do
      case (map Right xs) of
        [value] -> do
          writeLog h "Quote  = don't eval"
          pure $ value
        _ -> do
          writeLog h "This isn't eval list for quote"
          pure $ Left $ TEvalError "This isn't eval list for quote"
    SF EVAL -> do
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      case xs' of
        [Right value] -> do
          writeLog h $ " Eval this " ++ show value
          eval h $ Right value
        _ -> do
          writeLog h "This isn't eval list for eval"
          pure $ Left $ TEvalError "This isn't eval list for eval"
    SF CAR -> do
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      case xs' of
        [Right (TList xs'')] -> do
          writeLog h $ " Car this from LIst:  " ++ show xs'
          pure $ Right $ head xs''
        _ -> do
          writeLog h "This isn't eval list for car"
          pure $ Left $ TEvalError "This isn't eval list for car"
    SF CDR -> do
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      case xs' of
        [Right (TList xs'')] -> do
          writeLog h $ " CDR this from LIst:  " ++ show xs'
          pure $ Right $ TList $ tail xs''
        _ -> do
          writeLog h "This isn't eval list for CDR"
          pure $ Left $ TEvalError "This isn't eval list for cDr"
    SF CONS -> do
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      case xs' of
        [Right head', Right (TList tail')] -> do
          writeLog h $ " CONS this :  " ++ show xs'
          pure $ Right $ TList $ (head' : tail')
        _ -> do
          writeLog h "This isn't eval list for CONS"
          pure $ Left $ TEvalError "This isn't eval list for CONS"
    SF IF -> do -- bool FalseAction TrueAction ValueBool  - bool 1 2 False => 1
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      case xs' of
        [fAction, tAction, (Right ifValue)] -> do
          writeLog h $ " bool action action Bool:  " ++ show xs'
          if ifValue == TPil
          then pure $ tAction 
          else pure $ fAction
        _ -> do
          writeLog h "This isn't eval list for IF"
          pure $ Left $ TEvalError "This isn't eval list for IF"
    SF PRINT -> do
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      case xs' of
        [x] -> do
          hPrint h x
          pure $ Right $ TNil 
        otherwise -> do
          writeLog h "This isn't eval list for print"
          pure $ Left $ TEvalError "This isn't eval list for print"
    SF READ -> do
      if null xs then 
        eval h =<< hRead h
      else do
        writeLog h "This isn't eval list for read"
        pure $ Left $ TEvalError "This isn't eval list for read"
    SF COND -> do
      undefined
    SF SYMBOL -> do 
      xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
      case xs' of
        [x] -> do
          case x of
            Right (TStr symbol) -> pure $ Right $ TSymbol symbol 
            _ -> do
              writeLog h "This isn't string for symbol"
              pure $ Left $ TEvalError "This isn't eval list for symbol"
        otherwise -> do
          writeLog h "This isn't eval list for symbol"
          pure $ Left $ TEvalError "This isn't string for symbol"
    -- SF LAMBDA -> do
      -- xs' <- mapM (eval h) (map Right (drop 2 xs)) -- :: [EvalToken]
      -- case xs of
      --   [Arg, Body] -> do
          -- update h (head xs) value
          -- writeLog h (show (head xs) ++ " UPDATE TO " ++ show value )
          -- pure $ Right $ SF LAMBDA' Arg Body Ctx 
        -- _ -> do
        --   writeLog h $ "This isn't eval list for lambda" ++ show xs'
        --   pure $ Left $ TEvalError "This isn't eval list for lambda"
      -- xs' <- mapM (eval h) (map Right xs) -- :: [EvalToken]
    another -> do
      writeLog h $ "Head element evaless - case error " ++ show another ++ " here"
      xs' <- mapM (eval h) (map Right (xs)) -- :: [EvalToken]
      case xs' of
        [] -> pure $ Left $ func
        _ -> do
          writeLog h $ "Eval all " ++ show xs' ++ " , but return last"
          pure $ last xs'

evalNil :: (Monad m) => Handle m -> Token -> m (EvalToken)
evalNil h false = pure $ Right $ false
evalPil :: (Monad m) => Handle m -> Token -> m (EvalToken)
evalPil h true = pure $ Right $ true

evalSymbol :: (Monad m) => Handle m -> Token -> m (EvalToken)
evalSymbol h name = do
  writeLog h $ "evalSymbol: " ++ show name
  pure $ Right name


evalStr :: (Monad m) => Handle m -> Token -> m (EvalToken)
evalStr h string = pure $ Right $ string
--   value <- check h (TSymbol name)
--   case value of
funcOn :: (Token -> Token -> Token) -> EvalToken -> EvalToken -> EvalToken
funcOn f a b = do
  x <- a
  y <- b
  case f x y of
    TEvalError m -> Left $ TEvalError m
    token -> Right token
-- For Function with 1 parametr - SF
funcDuo :: (Token -> Token) -> EvalToken -> EvalToken
funcDuo f a = do
  x <- a
  case f x of
    TEvalError m -> Left $ TEvalError m
    token -> Right token
