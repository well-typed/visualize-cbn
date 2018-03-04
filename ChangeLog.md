# Revision history for visualize-cbn

## 0.1.0.1  -- 2018-03-04

* Start maintaining ChangeLog file.
* Minor improvement to the evaluation function:

    `let x = e1 in seq x e2`

  now takes a step to (provided that `e1 -> e1'`)

    `let x = e1' in seq x e2`

  this avoids moving `e1` to the heap  (provided that there aren't multiple
  references to `x` from `e2`), clarifying the evaluation.
* Added graph output (contributed by Yiğit Özkavcı).
