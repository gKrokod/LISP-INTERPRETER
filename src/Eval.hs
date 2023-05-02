module Eval where

import Types

eval :: SExpr -> SExpr
eval expr@(Number _) = expr
eval expr@(String _) = expr
eval expr@(Bool _) = expr
eval (List [Atom "quote", val]) = val
