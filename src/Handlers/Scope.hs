module Handlers.Scope where
import Types
import Prelude hiding (read)
data Handle m = Handle
  -- {   eval :: EvalToken -> m (EvalToken) 
    { writeLog :: String -> m ()
    , read :: Name -> m (Either EvalError Value)
    , update :: Name -> Value -> m ()
  }

-- type Name = Token -- TSymbol example
-- type Value = Token -- TSymbol example
-- type EvalError = Token
-- type EvalToken = Either EvalError Value

eval :: (Monad m) => Handle m -> EvalToken -> m (EvalToken)
eval h (Left x) = do
  writeLog h $ "EvalError man" ++ show x 
  pure $ Left x
eval h (Right x) = do
  case x of  
    TSymbol name -> do
      rh <- read h (TSymbol name)
      case rh of
        Left _ -> do
		  writeLog h $ "Ne nashli v baze" ++ show x 
		  pure $ Right x 
        Right v -> do 
		  writeLog h $ "nashli v baze " ++ show x 
		  pure $ Right v 
    _ -> do
      writeLog h $ "Ne TSymbol"
      pure $ Right x 
