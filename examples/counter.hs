fix = (\f -> f (@fix f))

mkCounter = (\self -> \n ->
    Counter (let n' = add n 1 in seq n' (self n'))
            n
      )

twice = (\self -> \n ->
    let c = @mkCounter self n
    in Counter (@tick c) (mul 2 n))

tick = (\c -> case c of {
      Counter t d -> t
    })

display = (\c -> case c of {
      Counter t d -> d
    })

main = @display (@tick (@tick (@tick (@fix @twice 0))))
