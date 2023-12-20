# Revision history for visualize-cbn

## 0.2.0 -- 2023-12-20

* Support multiple (mutually recursive) bindings in `let`
* Fix pattern matching on heap-allocated objects (we were losing sharing)
* Support heap inlining
* Support for selectors (`fst`, `snd`)
* Support the selector thunk optimization
* Add `--disable-ansi` command line
* Improve trace summarization
* Add some new primitive functions (`min`, `max`, `succ`)
* Add option to hide the prelude only after a specified step

## 0.1.0.2  -- 2019-09-10

* Newer GHC compatibility

## 0.1.0.1  -- 2018-03-04

* Start maintaining ChangeLog file.
* Minor improvement to the evaluation function:

    `let x = e1 in seq x e2`

  now takes a step to (provided that `e1 -> e1'`)

    `let x = e1' in seq x e2`

  this avoids moving `e1` to the heap  (provided that there aren't multiple
  references to `x` from `e2`), clarifying the evaluation.
* Added graph output (contributed by Yiğit Özkavcı).
* Improved heap descriptions (contributed by Tim Rakowski).
