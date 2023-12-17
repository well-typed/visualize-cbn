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
--
-- Annotated execution (as of dc51993):
--
--  2. As soon as we demand the value of @maxBV body_0@ to determine the
--     variable to be used for the outer-most lambda, this will force the
--     construction of the next term down. This happens recursively, so the
--     entire term is build in memory.
-- 10. This is an instructive subsequence: we will see the evaluation of
--     the simple term @lam (\y -> y)@.
-- 16. At this point this term is fully known: @Lam 1 (Var 1)@.
-- 17. The computation is driven by the computation of the variable to be used
--     for the outermost lambda; we can now continue this computation a little
--     bit, because we now know the @maxBV@ of the subterm @Lam 1 (Var 1)@.
-- 19. We repeat for the second simple term @lam (\z -> z)@.
-- 27. At this point we're almost done: we need to know the @max@BV@ of the
--     subterm @Var n_1@, but there aren't any, so that is just @0@.
-- 33. At this point all bound variables are known, and the new term has been
--     constructed.
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
