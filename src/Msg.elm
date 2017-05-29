module Msg exposing (..)

import Time exposing (Time)


type Msg
    = PianorollEvent Key
    | TriggerStart
    | Start Time
    | Tick Time
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
