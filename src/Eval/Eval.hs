module Eval.Eval where
import Types
import Parser (parseInput, clearComment)
import Text.Parsec (parse)
import Data.List ((\\))
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
bprim p (Number a) (Num b) = p (fromIntegral a) b
bprim p (Num a) (Number b) = p a (fromIntegral b)
bprim p (Num a) (Num b) = p a b
bprim p (String a) (String b) = p a b
bprim p (Bool a) (Bool b) = p a b
bprim p (List []) (List []) = p () ()  -- p [] [] - not compile
bprim p (List as) (List []) = False
bprim p (List []) (List bs) = False
bprim p (List (a : as)) (List (b : bs)) = case (bprim (==) a b) of
                                           False -> bprim p a b
                                           True -> bprim p (List as) (List bs)   
-- bprim p _ _ = error "bprim on undefined arguments"

boper :: (forall a. (Num a) => a -> a -> a) -> SExpr -> SExpr -> SExpr
boper func (Number a) (Number b) = Number $ func a b
boper func (Num a) (Num b) = Num $ func a b
boper func (Num a) (Number b) = Num $ func a (fromIntegral b)
boper func (Number a) (Num b) = Num $ func (fromIntegral a) b 
-- boper func (String a) (String b) = String $ a <> b 
boper (+) (String a) (String b) = String $ a <> b  -- concat
boper (-) (String a) (String b) = String $ a \\ b  -- list difference (non-associative)
-- boper _ _ _  = error "boper in undefined arguments"

--- возведение в степень любых чисел
bexpt:: SExpr -> SExpr -> SExpr
bexpt (Num a) (Num b) = Num $ a ** b
bexpt (Number a) (Num b) = Num $ (fromIntegral a) ** b
bexpt (Num a) (Number b) = Num $ a ** (fromIntegral b)
bexpt (Number a) (Number b) = Number $ a ^ b 

-- деление рациональных чисел
bdivNum:: SExpr -> SExpr -> SExpr
bdivNum (Num a) (Num b) = Num $ a / b
bdivNum (Number a) (Num b) = Num $ (fromIntegral a) / b
bdivNum (Num a) (Number b) = Num $ a / (fromIntegral b)
bdivNum (Number a) (Number b) = Num $ (fromIntegral a) / (fromIntegral b )
-- bdivNum _ _ = error "bdivmod in undefined arguments"

-- целочисленное деление и удаление
bdiv:: SExpr -> SExpr -> SExpr
bdiv (Number a) (Number b) = Number $ a `div` b 
-- bdiv _ _ = error "bdiv in undefined arguments"
bmod:: SExpr -> SExpr -> SExpr
bmod (Number a) (Number b) = Number $ a `mod` b 
-- bmod _ _ = error "bmod in undefined arguments"
