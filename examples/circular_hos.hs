-- See "Using Circular Programs for Higher-Order Syntax"
-- by Emil Axelsson and Koen Claessen (ICFP 2013)
-- <https://emilaxelsson.github.io/documents/axelsson2013using.pdf>
--
-- See Unfolder episode 17 for more details.
--
-- Suggested execution:
--
-- > cabal run visualize-cbn -- \
-- >   --show-trace \
-- >   --hide-prelude \
-- >   --gc \
-- >   --selector-thunk-opt \
-- >   --inline-heap \
-- >   --hide-inlining \
-- >   --hide-gc \
-- >   --hide-selector-thunk-opt \
-- >   --javascript foo.js \
-- >   -i examples/circular_hos.hs
maxBV = (\exp ->
    case exp of {
        Var x   -> 0
      ; App f e -> max (@maxBV f) (@maxBV e)
      ; Lam n f -> n
      }
  )

lam = (\f ->
     let {
         body = f (Var n)
       ; n    = succ (@maxBV body)
       }
     in seq n (Lam n body)
  )

main = @lam (\x -> App (App (@lam (\y -> y)) (@lam (\z -> z))) x)
