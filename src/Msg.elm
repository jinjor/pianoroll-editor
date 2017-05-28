module Msg exposing (..)


type Msg
    = PianorollEvent Key


type alias Key =
    { ctrl : Bool
    , shift : Bool
    , code : Int
    }
