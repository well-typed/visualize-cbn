enumFromTo = (\n -> \m ->
  if le n m then Cons n (@enumFromTo (add n 1) m)
            else Nil
)

length = (\acc -> \xs ->
  case xs of {
      Nil -> acc ;
      Cons x xs' -> @length (add 1 acc) xs'
    }
)

main = @length 0 (@enumFromTo 1 3)
