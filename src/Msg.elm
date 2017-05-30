module Msg exposing (..)

import Time exposing (Time)
import Core exposing (..)
import Midi exposing (Tick)


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
    | SetPosition Tick


type alias Mouse =
    { ctrl : Bool
    , shift : Bool
    , offset : Position
    }


type alias Key =
    { ctrl : Bool
    , shift : Bool
    , code : Int
    }
