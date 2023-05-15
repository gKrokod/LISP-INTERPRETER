module Handlers.Scope where
import Types

data Handle m = Handle {
    makeLocalEnvironment :: Environment -> Binding ->  m (Environment ) 
  , fullLocalEnvironment :: Environment -> Binding ->  m () 
  , clearEnvironment :: Environment -> m (Environment )
  , check :: Environment -> Name -> m (Maybe SExpr)
  , insert :: Environment -> Name -> SExpr -> m ()
  , update :: Environment -> Name -> SExpr -> m (SExpr) -- Bool
}
