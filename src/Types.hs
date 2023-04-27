module Types where


data SExpr = Atom String
           | List [SExpr]
           | Number Int 
           | String String
           | Bool Bool 
           | SForm SF
           | BOper BO
           | BPrim BP deriving (Show, Eq)

data SF    = DEF | SET | GET 
           | QUOTE | TYPEOF 
           | CONS | CAR | CDR | COND 
           | IF 
           | PRINT | READ 
           | EVAL  | EVALIN | LAMBDA
           | MACRO | MACROEXPAND deriving (Eq, Ord, Show )

data BO = ADD | SUB | MUL deriving (Eq, Show, Ord)
data BP = GT' | LT' | EQ' deriving (Eq, Show, Ord)
