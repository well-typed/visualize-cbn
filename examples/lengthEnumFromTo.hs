enumFromTo = (\n -> \m ->
  if le n m then Cons n (@enumFromTo (add n 1) m)
            else Nil
)

length = (\xs ->
  case xs of {
      Nil -> 0 ;
      Cons x xs' -> add 1 (@length xs')
    }
)

main = @length (@enumFromTo 1 3)
