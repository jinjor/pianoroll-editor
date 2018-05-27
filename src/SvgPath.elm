module SvgPath exposing (..)


start : Int -> Int -> String
start x y =
    "M" ++ String.fromInt x ++ "," ++ String.fromInt y


v : Int -> String -> String
v length s =
    s ++ "v" ++ String.fromInt length


h : Int -> String -> String
h length s =
    s ++ "h" ++ String.fromInt length
