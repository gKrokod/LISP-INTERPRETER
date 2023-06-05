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
atomExprToName x = error $ "vot tyt " ++  show x

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

boper :: (forall a. (Num a) => a -> a -> a) -> SExpr -> SExpr -> SExpr
boper func (Number a) (Number b) = Number $ func a b
boper _ _ _  = error "boper in undefined arguments"
