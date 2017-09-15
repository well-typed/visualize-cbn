# Call-by-name interpretation and visualization tool

Haskell and other call-by-name languages use _lazy evaluation_ as their default evaluation strategy. For beginners and advanced programmers alike this can sometimes be confusing. The `visualize-cbn` tool is designed to help in such cases; it is a simple interpreter for a mini-Haskell-like language which outputs the state of the program at every step in a human readable format. It can also generate a HTML/JavaScript version with "Previous" and "Next" buttons to allow to step through a program.

## Example

Consider the following example program:

``` haskell
fac = (\n ->
    if le n 1
      then 1
      else mul (@fac (sub n 1)) n
  )

main = @fac 1
```

The syntax is not _quite_ Haskell, but hopefully it should be pretty self-explanatory. The parser is pretty finicky; look at some of the examples in `examples/` to deduce what the syntax is. The only somewhat odd feature is the identifies marked with an at-sign (`@`); these corresponds to pointers in the heap. For programs in their initial state (i.e., as written down), the only heap pointers we expect are to CAFs (constant applicative forms; values defined at the top-level of the program).

## Stepping through

We can step through the evaluation of this program using

```
visualize-cbn -i examples/fac.hs --show-trace --hide-prelude
```

This will result in something like

```
** 0

fac 1

(apply fac)

** 1

if 1 <= 1
  then 1
  else fac (1 - 1) * 1

(delta: 1 <= 1)

** 2

if True
  then 1
  else fac (1 - 1) * 1

(if True)

** 3

1

(whnf)
```

At every step it lists the current state of the program, as well as the reduction rules that apply. There are some options for tweaking the output; see `--help`.

## Generating HTML/JavaScript

The tool can also generate HTML/JavaScript:

```
cr visualize-cbn -i examples/fac.hs --javascript foo.js
```

The resulting `.js` file can be embedded in a HTML page (such as a blog post); a minimal HTML page illustrating how this is done is given by

``` html
<html>
<body>

<a onclick="cbnPrev()">Prev</a>
<a onclick="cbnNext()">Next</a>
(step <span id="cbn_step">Step</span>, <span id="cbn_status">Status</span>)

<table width="100%" border="1" cellpadding="5" style="border-collapse: collapse;">
<tr><td><div style="font-family: monospace;" id="cbn_term">Term</div></td></tr>
<tr><td><div style="font-family: monospace;" id="cbn_heap">Heap</div></td></tr>
</table>

<script type="text/javascript" src="foo.js"></script>

</body>
</html>
```

(This `.html` file was not written to illustrate HTML best practices :-) ) See the Well-Typed blog post about the tool for an example output. 
