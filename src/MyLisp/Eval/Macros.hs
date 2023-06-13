module MyLisp.Eval.Macros where
import MyLisp.Types
import qualified Data.Map as Map
import MyLisp.Eval.Eval

mExpand :: MacroEnvironment -> SExpr -> SExpr
mExpand mEnv expr@(List [SForm LAMBDA, args, body]) = case Map.lookup expr mEnv of
  Just expr' -> expr'
  Nothing -> List [SForm LAMBDA, args', body'] 
  -- Nothing -> List [SForm LAMBDA, args', (mExpand mEnv' body)] 
    where mEnv' = Map.filterWithKey (\k _ -> k `notElem` dict ) mEnv 
          dict  = atomExprToMacroName args'
          args' = case Map.lookup args mEnv of -- если в аргументах макроса есть наши аргументы целиком, то заменяй
                    -- (macro name (( lambda name (+ 1 x)) 1) ) x -> (lambda x (+ 1 x)) 1
                    Just a -> a
                    Nothing -> args -- во время eval аргументы эти уже переделаются в список []
          body' = case Map.lookup body mEnv of -- заменяем целиком аргумент если есть в макросе, иначе не лезем внутрь
                    Just a -> a
                    Nothing -> mExpand mEnv' body

mExpand mEnv expr@(List [SForm MACRO, args, body]) = case Map.lookup expr mEnv of
  Just expr' -> expr'
  Nothing -> List [SForm MACRO, args', body'] 
  -- Nothing -> List [SForm MACRO, args', (mExpand mEnv' body)] 
    where mEnv' = Map.filterWithKey (\k _ -> k `notElem` dict ) mEnv    -- вот здесь пропадает body
    -- where mEnv' = mEnv --Map.filterWithKey (\k _ -> k `notElem` dict ) mEnv    -- вот здесь пропадает body
          dict  = atomExprToMacroName args'
          args' = case Map.lookup args mEnv of -- заменяем целиком аргумент если есть в макросе, иначе не лезем внутрь
                    Just a -> a
                    Nothing -> args
          body' = case Map.lookup body mEnv of -- заменяем целиком аргумент если есть в макросе, иначе не лезем внутрь
                    Just a -> a
                    Nothing -> mExpand mEnv' body

-- Скорее всего macro' никогда не встретится
mExpand mEnv expr@(SForm (MACRO' macroArgs body)) = case Map.lookup expr mEnv of
  Just expr' -> undefined --expr'
  Nothing -> undefined --SForm $ MACRO' macroArgs (mExpand mEnv' body)
    -- where mEnv' = Map.filterWithKey (\k _ -> k `notElem` dict ) mEnv 
    --       dict  = macroArgs
-- -- skoree vsego lambda' никогда не встретится
--
mExpand mEnv expr@(SForm (LAMBDA' args body env)) = case Map.lookup expr mEnv of
  Just expr' -> undefined --expr'
  Nothing -> undefined --SForm $ LAMBDA' args (mExpand mEnv' body) env 
    -- where mEnv' = Map.filterWithKey (\k _ -> k `notElem` dict ) mEnv 
    --       dict  = map Atom args
    --
mExpand mEnv expr@(List xs) = case Map.lookup expr mEnv of
  Just expr' -> undefined -- expr'
  Nothing -> List $ map (mExpand mEnv) xs

mExpand mEnv expr = case Map.lookup expr mEnv of
  Nothing -> expr
  Just expr' -> expr' --if show expr' == "defnn" then expr' else error $ show expr'  --expr'
-------------------------------------------------- формы до починки defn
-- mExpand :: MacroEnvironment -> SExpr -> SExpr -- версия работает, с защитой затенения, но аргумент не подставить
-- mExpand mEnv expr@(List [SForm LAMBDA, args, body]) = case Map.lookup expr mEnv of
--   Just expr' -> expr'
--   Nothing -> List [SForm LAMBDA, (mExpand mEnv args), (mExpand mEnv' body)] 
--     where mEnv' = Map.filterWithKey (\k _ -> k `notElem` dict ) mEnv 
--           dict  = atomExprToMacroName args
-- mExpand mEnv expr@(List [SForm MACRO, args, body]) = case Map.lookup expr mEnv of -- версия без залаза в аргументы
--   Just expr' -> expr'
--   Nothing -> List [SForm MACRO, args, (mExpand mEnv' body)] 
--     where mEnv' = Map.filterWithKey (\k _ -> k `notElem` dict ) mEnv 
--           dict  = atomExprToMacroName args

-------------------------------------------------- формы после eval
-- 
--------------------------------------------------
-- mExpand mEnv expr@(List [SForm DEF, Atom name, value]) = case Map.lookup expr mEnv of
--   Just expr' -> expr'
--   Nothing -> List [SForm DEF, (mExpandAtom name, (mExpand mEnv' value)] 
--     where mEnv' = Map.filterWithKey (\k _ -> k `notElem` dict ) mEnv 
--           dict  = atomExprToMacroName (Atom name)

-- mExpand mEnv expr@(SForm (LAMBDA' args body env)) = case Map.lookup expr mEnv of
--   Just expr' -> expr'
--   Nothing -> SForm $ LAMBDA' args (mExpand mEnv' body) env 
--     where mEnv' = Map.filterWithKey (\k _ -> k `notElem` dict ) mEnv 
--           dict  = map Atom args

-- mExpand mEnv expr@(SForm (MACRO' macroArgs body)) = case Map.lookup expr mEnv of
--   Just expr' -> expr'
--   Nothing -> SForm $ MACRO' macroArgs (mExpand mEnv' body)
--     where mEnv' = Map.filterWithKey (\k _ -> k `notElem` dict ) mEnv 
--           dict  = macroArgs
atomExprToMacroName :: SExpr -> [MacroName]
atomExprToMacroName (List xs) = xs --concatMap atomExprToMacroName xs
atomExprToMacroName expr = [expr]

