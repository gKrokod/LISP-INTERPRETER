module Eval.Macros where
import Types
import qualified Data.Map as Map

mExpand :: MacroEnvironment -> SExpr -> SExpr
mExpand mEnv expr@(List [SForm LAMBDA, args, body]) = case Map.lookup expr mEnv of
  Just expr' -> expr'
  Nothing -> List [SForm LAMBDA, args, (mExpand mEnv' body)] 
    where mEnv' = Map.filterWithKey (\k _ -> k `notElem` dict ) mEnv 
          dict  = atomExprToMacroName args
mExpand mEnv expr@(List [SForm MACRO, args, body]) = case Map.lookup expr mEnv of
  Just expr' -> expr'
  Nothing -> List [SForm MACRO, args, (mExpand mEnv' body)] 
    where mEnv' = Map.filterWithKey (\k _ -> k `notElem` dict ) mEnv 
          dict  = atomExprToMacroName args
-- Скорее всего macro' никогда не встретится
mExpand mEnv expr@(SForm (MACRO' macroArgs body)) = case Map.lookup expr mEnv of
  Just expr' -> expr'
  Nothing -> SForm $ MACRO' macroArgs (mExpand mEnv' body)
    where mEnv' = Map.filterWithKey (\k _ -> k `notElem` dict ) mEnv 
          dict  = macroArgs
-- skoree vsego lambda' никогда не встретится
mExpand mEnv expr@(SForm (LAMBDA' args body env)) = case Map.lookup expr mEnv of
  Just expr' -> expr'
  Nothing -> SForm $ LAMBDA' args (mExpand mEnv' body) env 
    where mEnv' = Map.filterWithKey (\k _ -> k `notElem` dict ) mEnv 
          dict  = map Atom args
mExpand mEnv expr@(List xs) = case Map.lookup expr mEnv of
  Just expr' -> expr'
  Nothing -> List $ map (mExpand mEnv) xs
mExpand mEnv expr = case Map.lookup expr mEnv of
  Nothing -> expr
  Just expr' -> expr'

atomExprToMacroName :: SExpr -> [MacroName]
atomExprToMacroName (List xs) = xs --concatMap atomExprToMacroName xs
atomExprToMacroName expr = [expr]

