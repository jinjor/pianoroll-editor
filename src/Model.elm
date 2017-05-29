module Model exposing (..)

import Dict exposing (Dict)


type alias Model =
    { notes : Dict Int Note
    , currentMeasure : Int
    , playing : Bool
    }


type alias Note =
    { note : Int
    , velocity : Int
    , position : Tick
    , length : Tick
    , selected : Bool
    }


type alias Tick =
    Int


type alias Measure =
    Float


initialNotes : List Note
initialNotes =
    [ Note 50 127 0 100 False
    , Note 60 127 120 100 False
    , Note 70 127 240 100 True
    , Note 80 127 360 100 False
    ]


init : Model
init =
    Model
        (initialNotes |> List.indexedMap (,) |> Dict.fromList)
        0
        False


getNotes : Model -> List Note
getNotes model =
    model.notes
        |> Dict.values


selectAll : Model -> Model
selectAll model =
    model.notes
        |> Dict.map
            (\key note ->
                { note | selected = True }
            )
        |> (\notes -> { model | notes = notes })
