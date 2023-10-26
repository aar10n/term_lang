data List : a =
    | Nil
    | Cons a (List'a)
    ;

    
f1 () = "hi"

f2 () = Cons 'h' $ Cons 'i' Nil

f3 () = ((Cons 'h') ((Cons 'i') Nil))

f4 () = (Cons 'h' (Cons 'i' Nil))
