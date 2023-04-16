module Parser (readIOREPL, readREPL) where

import Types
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad 

readIOREPL :: IO (Either ParseError Token)
readIOREPL = do
  putStr ">>> "
  inText <- getLine
  pure $ readREPL inText

readREPL :: String -> Either ParseError Token
readREPL text = makeTypeFromSymbol . filterComment <$> parse parseTToken "" text'
  where 
   txt = dropWhile (==' ') text
   isList = (not . null) txt && length (words txt) > 1 && head txt /= '('
   text' = if isList then '(' : txt ++ ")" else txt

filterComment :: OldToken -> Token
filterComment (TList xs) = TList $ foldr f [] xs
  where f (TComment _) acc = acc
        f x acc = x : acc
filterComment x = x 

type OldToken = Token
makeTypeFromSymbol :: OldToken -> Token
makeTypeFromSymbol (TSymbol "+") = BO ADD
makeTypeFromSymbol (TSymbol "-") = BO SUB
makeTypeFromSymbol (TSymbol "*") = BO MUL
makeTypeFromSymbol (TSymbol "/") = BO DIV
makeTypeFromSymbol (TSymbol "mod") = BO MOD
makeTypeFromSymbol (TSymbol "++") = BO CONCAT

makeTypeFromSymbol (TSymbol ">") = BP GT'
makeTypeFromSymbol (TSymbol "<") = BP LT'
makeTypeFromSymbol (TSymbol "=") = BP EQ'

makeTypeFromSymbol (TSymbol "def") = SF DEF
makeTypeFromSymbol (TSymbol "set!") = SF SET
makeTypeFromSymbol (TSymbol "get") = SF GET
-- makeTypeFromSymbol (TSymbol "quote") = SF QUOTE -- дублирует TQuote Token 
makeTypeFromSymbol (TSymbol "typeof") = SF TYPEOF
makeTypeFromSymbol (TSymbol "cons") = SF CONS
makeTypeFromSymbol (TSymbol "car") = SF CAR
makeTypeFromSymbol (TSymbol "cdr") = SF CDR
makeTypeFromSymbol (TSymbol "cond") = SF COND
makeTypeFromSymbol (TSymbol "print") = SF PRINT
makeTypeFromSymbol (TSymbol "read") = SF READ
makeTypeFromSymbol (TSymbol "eval") = SF EVAL
makeTypeFromSymbol (TSymbol "eval-in") = SF EVALIN
makeTypeFromSymbol (TSymbol "lambda") = SF LAMBDA
makeTypeFromSymbol (TSymbol "macro") = SF MACRO
makeTypeFromSymbol (TSymbol "macroexpand") = SF MACROEXPAND
makeTypeFromSymbol (TSymbol "nil") = TNil
makeTypeFromSymbol (TSymbol "t") = TPil
makeTypeFromSymbol (TList []) = TNil
makeTypeFromSymbol (TList xs) = TList $ map makeTypeFromSymbol xs
-- makeTypeFromSymbol (TSymbol "symbol") = SF SYMBOL  -- дублирует наверно тип TSymbol String
makeTypeFromSymbol xs = xs

parseAnyToken :: Parser Token
parseAnyToken = choice [parseTComment, parseTQuote, parseTNum, parseTList, parseTStr, parseTSymbol]

parseTInt :: Parser Token
parseTInt = do
  n <- many1 digit
  pure $ TInt $ read n 

parseTToken :: Parser Token
parseTToken = lexeme $ parseAnyToken

parseTStr :: Parser Token
parseTStr = do
  void $ char '"'
  str <- many $ noneOf "\"" 
  void $ char '"'
  pure $ TStr str

parseTComment :: Parser Token
parseTComment = do
  void $ char ';'
  cmt <- many $ noneOf ";" 
  void $ char ';'
  pure $ TComment cmt

parseTSymbol :: Parser Token
parseTSymbol = many1 ( noneOf ("() \n\t\"';" ++ ['0'.. '9'])) >>= pure . TSymbol

parseTQuote :: Parser Token
parseTQuote = char '\'' *> parseAnyToken >>= pure . TQuote
--
-- parseTPlus :: Parser Token
-- -- parseTPlus = char '+' *>  (pure $ TPlus) 
-- parseTPlus = char ',' *>  (pure $ TPlus) 
--
-- parseTMul ::  Parser Token
-- parseTMul = char '*' *>  (pure $ TMul) 

parseTList :: Parser Token 
parseTList = do
  void $ lexeme $ char '('
  e <- many $ lexeme $ parseAnyToken
  void $ lexeme $ char ')'
  return $ TList e
 
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"
--
--todo make for 1.0e24 form
--make for minus number
parseTNum :: Parser Token
parseTNum = do 
  h <- many1 digit
  (char '.' *> many digit >>= \t -> pure $ TDouble $ read (h ++ "." ++ t ++ "0")) <|> (pure $ TInt $ read h)

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x
