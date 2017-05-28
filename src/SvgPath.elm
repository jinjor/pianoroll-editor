module SvgPath exposing (..)


start : Int -> Int -> String
start x y =
    "M" ++ toString x ++ "," ++ toString y


v : Int -> String -> String
v length s =
    s ++ "v" ++ toString length


h : Int -> String -> String
h length s =
    s ++ "h" ++ toString length
