module Handlers.Scope where
import Types

data Handle m = Handle {
    makeLocalEnvironment :: Environment -> Binding ->  m (Environment ) 
  , clearEnvironment :: Environment -> m (Environment )
  , check :: Environment -> Name -> m (Maybe SExpr)

}
