break = (\xs ->
      case xs of {
          Nil -> Pair Nil Nil
        ; Cons x xs' ->
            if eq x 0
              then Pair Nil xs'
              else let b = @break xs'
                   in Pair (Cons x (fst b)) (snd b)
        }
    )

last = (\def -> \xs ->
      case xs of {
          Nil -> def
        ; Cons x' xs' -> @last x' xs'
        }
    )

main = let broken = @break (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 0 (Cons 5 (Cons 6 (Cons 7 (Cons 8 Nil)))))))))
       in eq (@last 0 (fst broken)) (@last 0 (snd broken))