tick = (\c -> case c of {
      Counter ti to d -> ti
    })

tock = (\c -> case c of {
      Counter ti to d -> to
    })

display = (\c -> case c of {
      Counter ti to d -> d
    })

fix = (\f -> let a = @fix f in f a)

mkCounter = (\self -> \n ->
    Counter (let n' = add n 1 in seq n' (self n'))
            (let n' = add n 1 in seq n' (self n'))
            n
      )

reconstruct = (\n -> Pair n 0)

comp = (\f -> \g -> \x -> f (g x))

ticktock = (\self -> \pair ->
    case pair of {
        Pair x y -> @mkCounter (@comp self @reconstruct) x
      })

main = @display (@tick (@tock (@tick (@fix @ticktock (Pair 0 0)))))

{-
  Here are the constructor calls after the first tick:

  fix ticktock (0, 0)
  ticktock (fix ticktock) (0, 0)
  mkCounter (fix ticktock . reconstruct) 0
  fix ticktock (reconstruct 1)
-}
