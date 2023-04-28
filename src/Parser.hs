module Parser where

import Types
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad 

-- удалить комментарии, которые открываются и закрываются символом ;
clearComment :: String -> String
clearComment = snd . foldr f (False, [])
  where f ';' (flag, acc) = (not flag, acc)
        f x (False, acc) = (False, x : acc)
        f x (True, acc) = (True, acc)


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#" 

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

parseNumber :: Parser SExpr
parseNumber = try (char '-' >> many1 digit >>= pure . Number . read . ("-" ++))
              <|> (many1 digit >>= pure . Number . read)

parseString :: Parser SExpr
parseString = do
  void $ char '"'
  str <- many $ noneOf "\""
  void $ char '"'
  pure $ String str

parseList :: Parser SExpr 
parseList = do
  void $ char '('
  e <- many $ lexeme $ parseAnySExpr
  void $ char ')'
  return $ List e


parseAtom :: Parser SExpr
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = [first] ++ rest
  pure $ case atom of
    "#t" -> Bool True 
    "#f" -> Bool False
    "def" -> SForm DEF
    "set!" -> SForm SET
    "get" -> SForm GET
    "type-of" -> SForm TYPEOF
    "cons" -> SForm CONS
    "car" -> SForm CAR
    "cdr" -> SForm CDR
    "cond" -> SForm COND
    "if" -> SForm IF
    "read" -> SForm READ
    "print" -> SForm PRINT
    "eval" -> SForm EVAL
    "eval-in" -> SForm EVALIN
    "l" -> SForm LAMBDA
    "macro" -> SForm MACRO
    "+" -> BOper ADD
    "-" -> BOper SUB
    "*" -> BOper MUL
    ">" -> BPrim GT'
    "<" -> BPrim LT'
    "==" -> BPrim EQ'
    otherwise -> Atom atom
  -- try (do {whitespace; parseAtom}) <|> ( --если в строке есть еще атомы, то верни последний, но э
  -- походу этим должен заниматься эвулятор
          -- pure $ case atom of
          --   "#t" -> Bool True 
          --   "#f" -> Bool False
          --   "def" -> SForm DEF
          --   "set!" -> SForm SET
          --   "get" -> SForm GET
          --   "type-of" -> SForm TYPEOF
          --   "cons" -> SForm CONS
          --   "car" -> SForm CAR
          --   "cdr" -> SForm CDR
          --   "cond" -> SForm COND
          --   "if" -> SForm IF
          --   "read" -> SForm READ
          --   "print" -> SForm PRINT
          --   "eval" -> SForm EVAL
          --   "eval-in" -> SForm EVALIN
          --   "l" -> SForm LAMBDA
          --   "macro" -> SForm MACRO
          --   "+" -> BOper ADD
          --   "-" -> BOper SUB
          --   "*" -> BOper MUL
          --   ">" -> BPrim GT'
          --   "<" -> BPrim LT'
          --   "==" -> BPrim EQ'
          --   otherwise -> Atom atom)

parseQuoted :: Parser SExpr
parseQuoted = char '\'' *> parseAnySExpr >>= \x -> pure $ List [Atom "quote", x] 

parseAnySExpr :: Parser SExpr
parseAnySExpr = do
  whitespace
  -- parseNumber <|> parseAtom <|> parseString<|> parseQuoted<|> parseList
  choice [parseNumber, parseString, parseQuoted, parseList, try parseAtom]



