builtin_add_int : Int -> Int -> Int
builtin_sub_int : Int -> Int -> Int


class Num : a
  | `+` : a -> a -> a
  | `-` : a -> a -> a
  ;

instance Num'Int
  | `+` = builtin_add_int
  | `-` = builtin_sub_int
  ;

main () = 1 + 2
