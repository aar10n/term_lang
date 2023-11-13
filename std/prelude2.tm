

data Bool;
data Char;   # 8-bit signed integer
data Int;    # 32-bit signed integer
data Double; # 64-bit IEEE 754 floating point


# ============ #
#     Num      #
# ============ #

class Num : a
  | `+` : a -> a -> a
  | `-` : a -> a -> a
  | fromInt : Int -> a
  ;


# Int
builtin_int_add : Int -> Int -> Int
builtin_int_sub : Int -> Int -> Int

instance Num'Int
  | `+` = builtin_int_add
  | `-` = builtin_int_sub
  | fromInt i = i
  ;

# Double
builtin_double_add : Double -> Double -> Double
builtin_double_sub : Double -> Double -> Double
builtin_double_from_int : Int -> Double

instance Num'Double
  | `+` = builtin_double_add
  | `-` = builtin_double_sub
  | fromInt = builtin_double_from_int
  ;


# ============== #
#      Frac      #
# ============== #

class Frac : a |Num'a|
  | `/` : a -> a -> a
  | fromDouble : Double -> a
  ;

# Double
builtin_double_div : Double -> Double -> Double

instance Frac'Double
  | `/` = builtin_double_div
  | fromDouble d = d
  ;

# ======================== #

main () = 1.5 + 1
