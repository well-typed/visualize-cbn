
fix = (\f -> let a = @fix f in f a)

fac = (\r -> \n ->
    if eq n 1
      then 1
      else let n' = sub n 1 in seq n' (mul n (r n')))

main = @fix @fac 5
