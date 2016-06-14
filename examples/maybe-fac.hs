bind_Maybe = (\ma -> \f ->
  case ma of {
      Nothing -> Nothing ;
      Just a  -> f a
    }
)

return_Maybe = (\a -> Just a)

fac = (\n ->
  if le n 1
    then Just 1
    else @bind_Maybe (@fac (sub n 1)) (\n' -> @return_Maybe (mul n n'))
)

main = @fac 5
