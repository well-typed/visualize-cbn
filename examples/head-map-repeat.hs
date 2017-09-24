-- head (map (\x -> x + x) (repeat (10 + 1)))

repeat = (\x -> let xs = @repeat x
                in Cons x xs)

map = (\f -> (\xs -> case xs of {
                       Nil -> Nil ;
                       Cons x xs' -> let fx = f x
                                     in Cons fx (@map f xs')
                     } ))
head = (\xs -> case xs of {
                 Cons x xs' -> x
               } )

main = @head (@map (\x -> add x x) (@repeat (add 10 1)))

