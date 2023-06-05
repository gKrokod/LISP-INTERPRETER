module Types where
import Control.Concurrent (MVar)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map

data SExpr = Atom String  -- +
           | List [SExpr] -- +
           | Number Int  -- +
           | String String -- +
           | Bool Bool  -- +
           | SForm SF -- +-
           | BOper BO -- +
           | BPrim BP 
           | Void deriving (Eq, Ord) --  затычка
 
data SF    = DEF | SET | GET  -- + + +
           | QUOTE | TYPEOF -- + +
           | CONS | CAR | CDR | COND -- + + + +
           | LIST
           -- | IF -- made in base library
           | PRINT | READ -- + +
           | EVAL  | LAMBDA -- + - +
           | LAMBDA' [Name] Value Environment -- lambda args body -> lambda' args body env -- +
           | MACRO 
           | MACRO' [MacroName] Value
           | EVALIN
           | MACROEXPAND deriving (Eq, Ord) -- -

-- для автоматического deriving data SF
instance Ord (Environment) where
  compare _ _ = EQ 

type MacroEnvironment = Map.Map MacroName Value 
type MacroName = SExpr

data BO = ADD | SUB | MUL deriving (Eq, Ord)
data BP = GT' | GTQ' | LT' | LTQ' | EQ' deriving (Eq, Ord)


instance Show BO where
  show ADD = "+"
  show SUB = "-"
  show MUL = "*"

instance Show BP where
  show GT' = ">"
  show GTQ' = ">="
  show LT' = "<"
  show LTQ' = "<="
  show EQ' = "=="

instance Show SExpr where
  show (Atom str) = str --"atom " ++ str
  show (List []) = "NIL"
  show (List xs) = "(" ++ intercalate " " (map show xs) ++ ")"
  show (Number int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Bool False) = "#f" 
  show (Bool True) = "#t" 
  show (SForm sf) = show sf
  show (BOper bo) = show bo
  show (BPrim bp) = show bp
  show Void = "{Void}"

instance Show SF where
  show DEF = "DEF"
  show GET = "GET"
  show SET = "SET"
  show TYPEOF = "TYPE-OF"
  show CONS = "CONS"
  show CAR = "CAR"
  show CDR = "CDR"
  show COND = "COND"
  show LIST = "LIST"
  -- show IF = "if"
  show READ = "READ"
  show PRINT = "PRINT"
  show EVAL = "EVAL"
  show EVALIN = "EVAL-IN"
  show LAMBDA = "LAMBDA"
  show (LAMBDA' xs v e) = "\\" ++ intercalate " " xs ++ " -> " ++ show v -- \x y z -> (+ x y z)
  show MACRO = "MACRO"
  show (MACRO' xs v ) = "@\\" ++ intercalate " " [(show xs)] ++ " -> " ++ show v -- @\x y z -> (+ x y z)
  -- show (MACRO' xs v ) = "macro " ++ show xs ++ " " ++ show v--"@\\" ++ intercalate " " [(show xs)] ++ " -> " ++ show v -- @\x y z -> (+ x y z)
  

-- окружение есть ящик содержащий фрейm
type Environment' a = MVar (Frame a) 
-- фрейм из ящика окружения есть таблица связывания (Frame a)
-- и новый ящик и объемлющего окружения (enclosing environment) 
data Frame a = Frame a (Environment' a) 
--синонимы
type Environment = Environment' Binding
type Binding = Map.Map Name Value
type Name = String --SExpr --Atom String 
type Value = SExpr

