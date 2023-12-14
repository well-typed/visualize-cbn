f = (\x -> @g x)
g = (\x -> @h x)
h = (\x -> succ x)

main = @f 1


