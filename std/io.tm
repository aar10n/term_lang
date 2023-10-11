panic : a -> never
println : String -> () ~ IO


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


test () = do
    | raise 1
    | println "hi"
    | ['a']
    ;
