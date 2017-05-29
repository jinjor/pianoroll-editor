module Midi exposing (..)

import Time exposing (Time)


type alias Tick =
    Int


type alias Measure =
    Float


tickToMeasure : Tick -> Measure
tickToMeasure tick =
    toFloat tick / 480


measureToTick : Measure -> Tick
measureToTick measure =
    floor (measure * 480)


positionToTime : Int -> Tick -> Time
positionToTime timeBase position =
    toFloat position * (1000.0 / (toFloat timeBase * 2.0))
