
class Stream : s
    | type Item
    | next : s -> Step's Item
    ;

data Step s =
    | Yield s<Item> s
    | Skip s
    | Done 
    ;


# stream : [a] -> Stream's a
stream xs = case xs
    | [] -> Done
    | x:xs -> Yield x (stream xs)
    ;

# unstream : Stream's a -> [a]
unstream s = case (next s)
    | Yield x s -> x : unstream s
    | Skip s -> unstream s
    | Done -> []
    ;

# map : (a -> b) -> Stream's a -> Stream's b
map f s = case (next s)
    | Yield x s -> Yield (f x) (map f s)
    | Skip s -> Skip (map f s)
    | Done -> Done
    ;

# filter : (a -> Bool) -> Stream's a -> Stream's a
filter p s = case (next s)
    | Yield x s -> if (p x) then Yield x (filter p s) else Skip (filter p s)
    | Skip s -> Skip (filter p s)
    | Done -> Done
    ;
