__builtin_put_char : Char -> ()
__builtin_get_char : () -> Char


effect IO
    | put_char : Char -> ()
    | get_char : () -> Char
    ;

default handler stdio for IO
    | put_char c k = k $ __builtin_put_char c
    | get_char k = k $ __builtin_get_char ()
    ;


prints x = case x
    | Cons c cs -> do
        | put_char c
        | prints cs
        ;
    | Nil -> ()
    ;

println x = do
    | prints $ show x
    | put_char '\n'
    ;


main () = do
    | println "Hello, world!"
    ;




# tail : List'a -> List'a;
tail xs = case xs
    | Nil -> error "tail: empty list"
    | Cons x xs -> xs
    ;

# append : List'a -> List'a -> List'a;
append xs ys = case xs
    | Nil -> ys
    | Cons x xs -> Cons x (append xs ys)
    ;




#id :: a -> a
id x = x


# id : forall a || a -> a

# test : forall a || Maybe'a -> a 
test Maybe'a = case x
    | Some x -> x
    | None -> panic("expected Some found None")
    ;
