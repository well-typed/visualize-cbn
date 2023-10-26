object = (\f -> f undefined (@object f))

mkCounter = (\super -> \this -> \n ->
      Counter (this (add 1 n)) n
    )

faster = (\super -> \this -> \n ->
      let c = super n
      in Counter (this (add 2 n)) (@value c)
    )

tick = (\c -> case c of {
      Counter t d -> t
    })

value = (\c -> case c of {
      Counter t d -> d
    })

mixin = (\f -> \g -> \super -> \this ->
      f (g super this) this
    )

main = @value (@tick (@tick (@object (@mixin @faster @mkCounter) 0)))
