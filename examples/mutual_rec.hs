-- Simple example of two mutually recursive functions
-- f x will return 0 if x is even and 1 if x is odd.
main =
     let {
         f = (\x -> if eq x 0 then 0 else g (sub x 1))
       ; g = (\x -> if eq x 0 then 1 else f (sub x 1))
     } in f 2