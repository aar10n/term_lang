panic : a || a -> never

effect Except : e
    | raise : e -> ()
    ;

handler unhandled_exception for Except'e : e
    | raise x _ = panic x
    ;

test () = raise 1
