module Core exposing (..)

-- POSITION


type alias Position =
    { x : Int
    , y : Int
    }



-- TUPLE


(=>) : a -> b -> ( a, b )
(=>) =
    (,)



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
