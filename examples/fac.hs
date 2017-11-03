fac = (\n ->
    if le n 1
      then 1
      else mul (@fac (sub n 1)) n
  )

main = @fac 1

