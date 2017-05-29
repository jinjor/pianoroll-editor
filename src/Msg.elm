module Msg exposing (..)


type Msg
    = PianorollEvent Key
    | Start
    | Stop
    | PrevMeasure
    | NextMeasure


type alias Key =
    { ctrl : Bool
    , shift : Bool
    , code : Int
    }
