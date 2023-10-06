data Bool = True | False;


# Classes

class Show : a
    | show : a -> String
    ;

class Eq : a
    | `==` : a -> a -> Bool
    ;

# Instances

instance Show'Int
    | show = show_int
    ;

instance Show'Char
    | show = show_char
    ;

instance Show'Bool
    | show x = case x
        | True -> "True"
        | False -> "False"
        ;
    ;

instance Show'String
    | show s = s
    ;


map : (a -> b) -> List'a -> List'b
map f xs = case xs
    | Cons x xs -> Cons (f x) (map f xs)
    | Nil -> Nil
    ;

front : List'a -> a ~ Except'String
front xs = case xs
    | Cons x _ -> x
    | Nil -> throw "Empty list"
    ;


inc_with_print : Int -> Int ~ IO
inc_with_print x = do
    | println x
    | x + 1
    ;

map (x => x + 1) [1, 2, 3] : List'Int
map inc_with_print [1, 2, 3] : List'(Int ~ IO)

# we have to 'handle' the effect
