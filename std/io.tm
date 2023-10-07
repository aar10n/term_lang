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

data IOError =
    | MkIOError String
    ;

