panic : a -> never
builtin_add_int : Int -> Int -> Int
builtin_sub_int : Int -> Int -> Int
builtin_put_char : Char -> () ~ Except'IOError
builtin_get_char : () -> Char ~ Except'IOError 

data List : a =
    | Nil
    | Cons a (List'a)
    ;


class Num : a
  | `+` : a -> a -> a
  | `-` : a -> a -> a
  ;

instance Num'Int
  | `+` = builtin_add_int
  | `-` = builtin_sub_int
  ;


effect Except : e
    | raise : e -> ()
    ;

default handler panic_exception for Except'e
    | raise = panic
    ;


data IOError = IOError { msg : String };

# you can specify a list of side effects after a `~` which may occur in handlers of the effect
effect IO ~ Except'IOError
    | read_char : () -> Char
    | write_char : Char -> ()
    ;

default handler stdio for IO
    | read_char = builtin_get_char
    | write_char = builtin_put_char
    ;


println s = 
    handle case s
        | [] -> write_char '\n'
        | x:xs -> write_char x; println xs
        ;
    | Except'IOError ~> panic_exception
    ;

# the effect operator `~` binds default handers to effects in the applied expression
purefn x = do
    | ~println "hi"
    | x + 1
    ;

main () = purefn 2
