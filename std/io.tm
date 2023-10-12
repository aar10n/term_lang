panic : a -> never
println : String -> () ~ IO
builtin_put_char : Char -> () ~ Except'IOError
builtin_get_char : () -> Char ~ Except'IOError


data List : a =
    | Nil
    | Cons a (List'a)
    ;

effect Except : e
    | raise : e -> ()
    ;

effect IO
    | read_char : () -> Char ~ Except'IOError
    | write_char : Char -> () ~ Except'IOError
    ;

data IOError = IOError { msg : String };

default handler stdio for IO
    | read_char = builtin_put_char
    | write_char = builtin_get_char
    ;


# the effect operator `~` handled effects in the applied expression
# by all applicable handlers. 
test () = do
    | ~println "hi"
    | 1
    ;

#test () = do
#    | handle (println "hi");
#    ;

# -----------

#test () k = do
#    | handle (println "hi")
#        | Except'IOError ~> 
#        | IO ~> 
#        | k 1
#        ;
#    |
#    ;
