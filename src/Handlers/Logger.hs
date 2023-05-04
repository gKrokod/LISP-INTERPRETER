module Handlers.Logger where
import qualified Data.Text as T

data Handle m = Handle {
  writeLog :: T.Text -> m ()
}

