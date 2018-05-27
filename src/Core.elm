module Core exposing (..)

import Time exposing (Posix)



-- POSITION


type alias Position =
    { x : Int
    , y : Int
    }



-- LIST


splitWhile : (a -> Bool) -> List a -> ( List a, List a )
splitWhile f list =
    splitWhileHelp f [] list


splitWhileHelp : (a -> Bool) -> List a -> List a -> ( List a, List a )
splitWhileHelp f taken list =
    case list of
        [] ->
            ( taken, [] )

        x :: xs ->
            if f x then
                splitWhileHelp f (x :: taken) xs

            else
                ( taken, list )



-- TIME


diffMillis : Posix -> Posix -> Float
diffMillis from to =
    (Time.posixToMillis to - Time.posixToMillis from)
        |> toFloat
