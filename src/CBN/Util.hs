module CBN.Util (
    deleteKeysFromMap
  ) where

deleteKeysFromMap :: Ord k => [k] -> Map k a -> Map k a
deleteKeysFromMap []     = id
deleteKeysFromMap (k:ks) = deleteKeys ks . Map.delete k
