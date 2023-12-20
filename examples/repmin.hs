-- The classic repMin circular program due to Richard Bird.
-- See Unfolder episode 17 for more details.
--
-- Suggested execution:
--
-- > cabal run visualize-cbn -- \
-- >   --show-trace \
-- >   --hide-prelude=1 \
-- >   --gc \
-- >   --selector-thunk-opt \
-- >   --inline-heap \
-- >   --hide-inlining \
-- >   --hide-gc \
-- >   --hide-selector-thunk-opt \
-- >   --javascript foo.js \
-- >   -i examples/repmin.hs
--
-- Annotated execution (as of dc51993):
--
--  1. One way to think about this circular program is to consider that it
--     first creates a pointer to an int (the new value in the leaves), and
--     then starts building up a tree with all leaves pointing to this int;
--     as it builds the tree, it is also computing the value of this int.
--  6. We're starting to see the tree take shape here; the top-level structure
--     of the tree is now known.
-- 10. Similarly, we now see the shape of the left subtree.
-- 13. Here we see the first @Leaf@, ponting to @m_1@; part of the computation
--     of @m_1@ is now also known (@mb_7@).
-- 16. The second @Leaf@ is known.
-- 18. The minimum value of the left subtree is known (@mb_4@).
-- 28. At this point the structure of the tree is mostly done. We can
--     finish the value computation.
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
