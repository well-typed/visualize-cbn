-- Pairs

fst = (\pair -> case pair of { Pair x y -> x })
snd = (\pair -> case pair of { Pair x y -> y })

-- Lazy state monad

ret = (\a -> \s -> Pair a s)

bind = (\x -> \f -> \s ->
  let a_s' = x s
  in f (@fst a_s') (@snd a_s')
)

mapM = (\f -> \as ->
  case as of {
      Nil        -> @ret Nil ;
      Cons a as' -> @bind (f a) (\b -> @bind (@mapM f as') (\bs -> @ret (Cons b bs)))
    }
)

get = (\s -> Pair s s)

put = (\new -> \old -> Pair Unit new)

evalState = (\k -> \s -> @fst (k s))

-- The actual computations
-- TODO: The "@"s here are the only difference from real haskell code;
-- if we got rid of those, we could actually copy and paste these into ghc :/

foo1 = (\n ->
    @bind get (\sumSoFar ->
    @bind (if eq sumSoFar 0
             then let sumSoFar' = add sumSoFar 1
                  in seq sumSoFar' (@put sumSoFar')
             else @ret Unit) (\unused ->
    ret n))
)

foo2 = (\n ->
    @bind get (\sumSoFar ->
    if eq sumSoFar 0
      then let sumSoFar' = add sumSoFar 1
           in seq sumSoFar' (@bind (@put sumSoFar') (\unused -> @ret n))
      else @ret n)
)

-- Top-level

main = 1
