panic : a -> never
builtin_put_char : Char -> () ~ Except'IOError
builtin_get_char : () -> Char ~ Except'IOError 

data List : a =
    | Nil
    | Cons a (List'a)
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
    handle 
        case s
        | [] -> write_char '\n'
        | x:xs -> write_char x; println xs
        ;
    | Except'IOError ~> panic_exception
    ;


# the effect operator `~` binds default handers to effects in the applied expression
#foo () = println "hi"
#bar () = ~println "hi"


