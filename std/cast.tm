builtin_itof : Int -> Float
builtin_dtoi : Float -> Int

builtin_add_int : Int -> Int -> Int
builtin_sub_int : Int -> Int -> Int

data Char;   # 8-bit signed integer
data Int;    # 32-bit signed integer
data Float;  # 32-bit IEEE 754 floating point
data Double; # 64-bit IEEE 754 floating point

# ============ #
#     Cast     #
# ============ #

class Cast : a b
  | cast : a -> b
  ;

instance Cast'Int Float
  | cast = builtin_itof 
  ;

# ============ #
#     Num      #
# ============ #

class Num : a
  | `+` : a -> a -> a
  | `-` : a -> a -> a
  ;

instance Num'Int
  | `+` = builtin_add_int
  | `-` = builtin_sub_int
  ;


# ======================== #

main () = 1.5 + 1
