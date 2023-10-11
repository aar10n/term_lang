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


panic : a -> never
println : String -> () ~ IO


test () = handle (raise 1)
    | Except'Int ~> { raise = k _ => k () }
    | () ~> println "ok"
    ;
