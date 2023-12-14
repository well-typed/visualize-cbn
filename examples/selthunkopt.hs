-- Demonstration of the need for the selector thunk optimization
-- This is the example from "Fixing some space leaks with a garbage collector".

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

-- strict version of concat (makes the example more clear)
concat = (\xs -> \ys ->
      case xs of {
          Nil        -> ys
        ; Cons x xs' -> let r = @concat xs' ys in seq r (Cons x r)
        }
    )

surprise = (\xs ->
      let b = @break xs
      in @concat (fst b) (@concat (Cons 4 (Cons 5 (Cons 6 Nil))) (snd b))
    )

main = @surprise (Cons 1 (Cons 2 (Cons 3 (Cons 0 (Cons 7 (Cons 8 (Cons 9 Nil)))))))
