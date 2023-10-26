object = (\f -> f (@object f))

mkCounter = (\this -> \n ->
      Counter (this (add 1 n)) n
    )

tick = (\c -> case c of {
      Counter t d -> t
    })

value = (\c -> case c of {
      Counter t d -> d
    })

main = @value (@tick (@tick (@object @mkCounter 0)))
