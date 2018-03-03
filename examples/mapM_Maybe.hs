bind = (\ma -> \f ->
  case ma of {
      Nothing -> Nothing ;
      Just a  -> f a
    }
)

return = (\a -> Just a)

enumFromTo = (\n -> \m ->
  if le n m then Cons n (@enumFromTo (add n 1) m)
            else Nil
)

mapM = (\f -> \xs ->
  case xs of {
      Nil        -> @return Nil ;
      Cons x xs' -> @bind (f x)         (\x'  ->
                    @bind (@mapM f xs') (\xs' ->
                    @return (Cons x' xs') ))
    }
)

main = @mapM @return (@enumFromTo 1 3)
