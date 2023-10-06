panic : a || a -> never

data List : a =
    | Nil
    | Cons a (List'a)
    ;

# cons : a -> List'a -> List'a;
cons x xs = Cons x xs

# head : List'a -> a;
head xs = case xs
    | Nil -> panic "head: empty list"
    | Cons x xs -> x
    ;
