-- | Snoc-lists
--
-- Intended for double import
--
-- > import CBN.Util.Snoc (Snoc)
-- > import qualified CBN.Util.Snoc as Snoc
module CBN.Util.Snoc (
    Snoc(..)
  , fromList
  ) where

data Snoc a = Nil | Cons (Snoc a) a
  deriving (Show, Eq, Ord)

fromList :: [a] -> Snoc a
fromList = foldl Cons Nil
