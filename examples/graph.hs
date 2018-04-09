ls = (Cons 1 (Cons 2 (Cons 3 @ls)))

double = (\n -> add n n)

main = @double (add 1 2)
