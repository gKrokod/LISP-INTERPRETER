module Handlers.Scope where
import Types

data Handle m a = Handle {
    makeLocalEnvironment :: Environment a -> a ->  m (Environment a) 
  , clearEnvironment :: Environment a -> m (Environment a)

}
