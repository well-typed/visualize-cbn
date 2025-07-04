-- example that unzip does some work before returning the result pair

-- some generator which does some work before producing the elements of a list
generate = (\x -> \y ->
  if le y 0 then Nil else
  if le x 0 then Nil else
  Cons (Pair x y) (@generate (sub x 1) (sub y 1))
  )

unzip = (\xs -> case xs of {
  Nil -> Pair Nil Nil;
  Cons xy xys -> case xy of {
    Pair x y -> let r = @unzip xys in Pair (Cons x (fst r)) (Cons y (snd r))
  }
  })

main = @unzip (@generate 1 2)
