object = (\f -> f (@object f))

mkCounter = (\this -> \n ->
      Counter (this (add 1 n)) n
    )

faster = (\this -> \n ->
      let c = @mkCounter this n
      in Counter (this (add 2 n)) (@value c)
    )

tick = (\c -> case c of {
      Counter t d -> t
    })

value = (\c -> case c of {
      Counter t d -> d
    })

main = @value (@tick (@tick (@object @faster 0)))
