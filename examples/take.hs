-- program starts here (lazy-ones.hs)
enumFromTo = (\n -> \m ->
  if le n m then Cons n (@enumFromTo (add n 1) m)
            else Nil
)

take = (\n -> \xs ->
           if eq n 0
             then Nil
             else case xs of {
                    Nil -> Nil ;
                    Cons x xs' -> Cons x (@take (sub n 1) xs')
                  }
       )

main = @take 1 (@enumFromTo 1 10)
