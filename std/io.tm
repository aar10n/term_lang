panic : a -> never
println : String -> () ~ IO
builtin_put_char : Char -> ()
builtin_get_char : () -> Char


data List : a =
    | Nil
    | Cons a (List'a)
    ;

effect Except : e
    | raise : e -> ()
    ;

effect IO
    | read_char : () -> Char
    | write_char : Char -> ()
    ;

data IOError = IOError { msg : String };

default handler stdio for IO
    | read_char = builtin_get_char
    | write_char = builtin_put_char
    ;


# the effect operator `~` binds default handers to effects in the applied expression
foo () = println "hi"
bar () = ~println "hi"

#bar () k = do
#    | x = handle (println "hi")
#        | IO ~> stdio
#        ;
#    | k x
#    ;

#test () k = do
#    | handle (println "hi")
#        | Except'IOError ~> 
#        | IO ~> 
#        | k 1
#        ;
#    |
#    ;
