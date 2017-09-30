const = (\x -> \y -> x)

main = (\x -> @const x x) ((\x -> @const x x) (@const 1 1))
