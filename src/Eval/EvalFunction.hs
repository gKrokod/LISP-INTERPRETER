module Eval.EvalFunction where
import Types
import Data.List
import Data.Function

funcBOMUL :: Token -> Token -> Token
funcBOMUL (TDouble x) (TDouble y) = TDouble (x * y)
funcBOMUL (TInt x) (TInt y) = TInt (x * y)
funcBOMUL (TDouble x) (TInt y) = TDouble (x * fromIntegral y)
funcBOMUL (TInt x) (TDouble y) = TDouble (fromIntegral x * y)
funcBOMUL (TPil) x = x-- True * Value = Value
funcBOMUL x (TPil) = x
funcBOMUL _ _ = TEvalError "Wrong List to mul"  
              
funcBOADD :: Token -> Token -> Token
funcBOADD (TDouble x) (TDouble y) = TDouble (x + y)
funcBOADD (TInt x) (TInt y) = TInt (x + y)
funcBOADD (TDouble x) (TInt y) = TDouble (x + fromIntegral y)
funcBOADD (TInt x) (TDouble y) = TDouble (fromIntegral x + y)
funcBOADD (TStr s1) (TStr s2) = TStr $ mconcat [s1,"\n",s2]
funcBOADD (TNil) x = x -- Empty List (or False) + Value = Value
funcBOADD x (TNil) = x
funcBOADD _ _ = TEvalError "Wrong List to add"  

funcBOSUB :: Token -> Token -> Token
funcBOSUB (TDouble x) (TDouble y) = TDouble (x - y)
funcBOSUB (TInt x) (TInt y) = TInt (x - y)
funcBOSUB (TDouble x) (TInt y) = TDouble (x - fromIntegral y)
funcBOSUB (TInt x) (TDouble y) = TDouble (fromIntegral x - y)
funcBOSUB (TStr s1) (TStr s2) = TStr $ s1 \\ s2 
funcBOSUB (TNil) (TInt x) = TInt $ (-1) * x -- Empty List (or False) - Value = -Value
funcBOSUB (TNil) (TDouble x) = TDouble $ (-1) * x -- Value - Empty List (or False) = Value
funcBOSUB x (TNil) = x
funcBOSUB _ _ = TEvalError "Wrong List to sub"  

funcBODIV :: Token -> Token -> Token
funcBODIV (TDouble x) (TDouble y) = TDouble (x / y)
funcBODIV (TInt x) (TInt y) = TInt (x `div` y) --TDouble $ ((/) `on` fromIntegral) x  y
funcBODIV (TDouble x) (TInt y) = TDouble (x / fromIntegral y)
funcBODIV (TInt x) (TDouble y) = TDouble (fromIntegral x / y)
-- funcBODIV (TPil) x = x-- True * Value = Value
funcBODIV x (TPil) = x
funcBODIV _ _ = TEvalError "Wrong List to div"  

funcBOMOD :: Token -> Token -> Token
funcBOMOD (TDouble x) (TDouble y) = TInt $ (mod `on` round) x y
funcBOMOD (TInt x) (TInt y) = TInt $ x `mod` y
funcBOMOD (TDouble x) (TInt y) = TInt $ ( round x `mod` y)
funcBOMOD (TInt x) (TDouble y) = TInt $ ( x `mod` round y)
funcBOMOD _ _ = TEvalError "Wrong List to mod"  

funcBOCONCAT :: Token -> Token -> Token
funcBOCONCAT (TStr s1) (TStr s2) = TStr $ s1 <> s2 
funcBOCONCAT _ _ = TEvalError "Wrong List to concat"  

funcBPGT :: Token -> Token -> Token
funcBPGT (TDouble x) (TDouble y) = if x > y then TPil else TNil 
funcBPGT (TDouble x) (TInt y) = if x > (fromIntegral y) then TPil else TNil 
funcBPGT (TInt x) (TInt y) = if x > y then TPil else TNil 
funcBPGT (TInt x) (TDouble y) = if (fromIntegral x) > y then TPil else TNil 
funcBPGT (TStr s1) (TStr s2) = if s1 > s2 then TPil else TNil 
funcBPGT (TList xs) (TList xs') = if xs > xs' then TPil else TNil 
funcBPGT x TNil = if x == TNil then TNil else TPil
funcBPGT TNil x = if x == TNil then TNil else TPil
funcBPGT _ _ = TEvalError "Wrong List to > "  

funcBPLT :: Token -> Token -> Token
funcBPLT = flip funcBPGT

funcBPEQ :: Token -> Token -> Token
funcBPEQ x y = if x == y then TPil else TNil 

funcSFTYPEOF :: Token -> Token
funcSFTYPEOF (TDouble _) = TStr "Double" 
funcSFTYPEOF (TInt _) = TStr "Integer" 
funcSFTYPEOF (TStr _) = TStr "String" 
funcSFTYPEOF (TList _) = TStr "List" 
funcSFTYPEOF (TNil) = TStr "Bool" 
funcSFTYPEOF (TPil) = TStr "Bool" 
funcSFTYPEOF (TSymbol _) = TStr "Symbol" 
funcSFTYPEOF (TEvalError _) = TStr "Error" 
funcSFTYPEOF (BO _) = TStr "BO" 
funcSFTYPEOF (SF _) = TStr "SF" 
funcSFTYPEOF (BP _) = TStr "BP" 

sfPrint :: EvalToken -> String
sfPrint (Left e) = show e 
sfPrint (Right v) = show v 
