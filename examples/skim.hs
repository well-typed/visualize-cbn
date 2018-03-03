
fix = (\f -> let a = @fix f in f a)

fac = (\r -> \n ->
    if eq n 1
      then 1
      else let n' = sub n 1 in seq n' (mul n (r n')))

skim = (\r -> \n ->
    if eq n 1
      then 1
      else sub (@fac r n) 1)

main = @fix @skim 5
