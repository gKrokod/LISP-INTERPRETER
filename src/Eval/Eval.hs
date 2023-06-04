module Eval.Eval where
import Types
import Parser (parseInput, clearComment)
import Text.Parsec (parse)
-- data Handle m = Handle {
--     scope :: Handlers.Scope.Handle m
--   , logger :: Handlers.Logger.Handle m
--   , hPrint :: SExpr -> m ()
--   , hRead :: m (SExpr)
-- }
--
--
atomExprToName :: SExpr -> [Name]
atomExprToName (Atom str) = [str]
atomExprToName (List xs) = concatMap atomExprToName xs

hPrint :: SExpr -> IO ()
hPrint = print 

hRead :: IO (SExpr)
hRead = do
  putStr ">>read>> "
  input <- clearComment <$> getLine
  case parse parseInput "lisp" input of
    Left e -> hRead
    Right msg -> pure msg

bprim :: (forall a. Ord a => a -> a -> Bool) -> SExpr -> SExpr -> Bool
bprim p (Number a) (Number b) = p a b
bprim p (String a) (String b) = p a b
bprim p (Bool a) (Bool b) = p a b
bprim p (List []) (List []) = p () ()  -- p [] [] - not compile
bprim p (List as) (List []) = False
bprim p (List []) (List bs) = False
bprim p (List (a : as)) (List (b : bs)) = case (bprim (==) a b) of
                                           False -> bprim p a b
                                           True -> bprim p (List as) (List bs)   
bprim p _ _ = error "bprim on undefined arguments"

boper :: (forall a. Num a => a -> a -> a) -> SExpr -> SExpr -> SExpr
boper func (Number a) (Number b) = Number $ func a b
boper _ _ _  = error "boper in undefined arguments"

-- boper ::
--
-- unBoxNumber :: SExpr -> Int 
-- unBoxNumber (Number x) = x
-- unBoxNumber _ = error "not Number"
--
-- unBoxString :: SExpr -> String 
-- unBoxString (String x) = x
-- unBoxBool :: SExpr -> Bool 
-- unBoxBool (Bool x) = x
-- unBoxAtom :: SExpr -> String 
-- unBoxAtom (Atom x) = x
-- -- unBoxList :: SExpr -> SExpr
-- -- unBoxList (List xs) = xs
-- --
--
-- apply :: (Monad m) => Handle m -> Environment -> SFunc -> m ([SExpr]) -> m SExpr
-- apply h env (BOper f) xs = do
--   L.writeLog (logger h) "apply BOper func. If empty list = error" 
--   xs' <- xs
--   let unboxXs = map unBoxNumber xs'
--   case f of
--     ADD -> pure $ Number $ foldl1' (+) unboxXs
--     SUB -> pure $ Number $ foldl1' (-) unboxXs
--     MUL -> pure $ Number $ foldl1' (*) unboxXs
-- -- Add Gt, LT, EQ for another Types
