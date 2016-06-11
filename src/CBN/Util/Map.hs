module CBN.Util.Map (
    deleteKeys
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

deleteKeys :: Ord k => [k] -> Map k a -> Map k a
deleteKeys []     = id
deleteKeys (k:ks) = deleteKeys ks . Map.delete k
