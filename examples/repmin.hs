-- The classic repMin circular program due to Richard Bird.
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
-- >   -i examples/repmin.hs
worker = (\m -> \t ->
      case t of {
          Leaf x -> Pair x (Leaf m)
        ; Branch l r ->
            let {
                resultLeft  = @worker m l
              ; resultRight = @worker m r
              ; mb          = min (fst resultLeft) (fst resultRight)
              }
            in seq mb (Pair mb (Branch (snd resultLeft) (snd resultRight)))
        }
    )

repMin = (\t ->
      let result = @worker (fst result) t
      in snd result
    )

main = @repMin (Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 3) (Leaf 4)))
