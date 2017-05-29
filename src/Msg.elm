module Msg exposing (..)


type Msg
    = PianorollEvent Key
    | Start
    | Stop
    | PrevMeasure
    | NextMeasure
    | MouseDownOnNote Int Mouse
    | MoveSelectedNotes Int
    | SelectArrowMode
    | SelectPenMode


type alias Mouse =
    { ctrl : Bool
    , shift : Bool
    }


type alias Key =
    { ctrl : Bool
    , shift : Bool
    , code : Int
    }
