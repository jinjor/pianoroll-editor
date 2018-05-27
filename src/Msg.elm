module Msg exposing (..)

import Core exposing (..)
import Midi exposing (Tick)
import Time exposing (Posix)


type Msg
    = PianorollEvent Key
    | TriggerStart
    | Start Posix
    | Tick Posix
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
