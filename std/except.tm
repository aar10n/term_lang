panic : a || a -> never

effect Except : e
    | raise : e -> ()
    ;

default handler unhandled_exception for Except'e : e
    | raise x _ = panic x
    ;


test () = handle (raise 1) 
    | Except'e ~> unhandled_exception
    ;


test () = handle (raise 1) 
    | Except'e ~> {
            "raise" => x k 
        }
    ;
