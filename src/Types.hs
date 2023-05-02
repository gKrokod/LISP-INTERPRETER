module Types where
import Control.Concurrent (MVar)
import Data.List (intercalate)

data SExpr = Atom String
           | List [SExpr]
           | Number Int 
           | String String
           | Bool Bool 
           | SForm SF
           | BOper BO
           | BPrim BP deriving (Eq, Ord)

data SF    = DEF | SET | GET 
           | QUOTE | TYPEOF 
           | CONS | CAR | CDR | COND 
           | IF 
           | PRINT | READ 
           | EVAL  | EVALIN | LAMBDA
           | MACRO | MACROEXPAND deriving (Eq, Ord )

data BO = ADD | SUB | MUL deriving (Eq, Ord)
data BP = GT' | LT' | EQ' deriving (Eq, Ord)

instance Show BO where
  show ADD = "+"
  show SUB = "-"
  show MUL = "*"

instance Show BP where
  show GT' = ">"
  show LT' = "<"
  show EQ' = "=="

instance Show SExpr where
  show (Atom str) = str
  show (List xs) = "(" ++ intercalate " " (map show xs) ++ ")"
  show (Number int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Bool False) = "#f" 
  show (Bool True) = "#t" 
  show (SForm sf) = show sf
  show (BOper bo) = show bo
  show (BPrim bp) = show bp

instance Show SF where
  show DEF = "def"
  show GET = "get"
  show SET = "set!"
  show TYPEOF = "type-of"
  show CONS = "cons"
  show CAR = "car"
  show CDR = "cdr"
  show COND = "cond"
  show IF = "if"
  show READ = "read"
  show PRINT = "print"
  show EVAL = "eval"
  show EVALIN = "eval-in"
  show LAMBDA = "l"
  show MACRO = "macro"

-- окружение есть ящик содержащий фрейm
type Environment a = MVar (Frame a) 
-- фрейм из ящика окружения есть таблица связывания (Frame a)
-- и новый ящик и объемлющего окружения (enclosing environment) 
data Frame a = Frame a (Environment a)
