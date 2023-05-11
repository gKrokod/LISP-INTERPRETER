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
hPrint :: SExpr -> IO ()
hPrint = print 

hRead :: IO (SExpr)
hRead = do
  putStr ">>read>> "
  input <- clearComment <$> getLine
  case parse parseInput "lisp" input of
    Left e -> hRead
    Right msg -> pure msg
