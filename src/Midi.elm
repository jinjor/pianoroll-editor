module Midi exposing (..)


type alias Tick =
    Int


type alias Measure =
    Float


defaultTimeBase : Int
defaultTimeBase =
    480


defaultTempo : Float
defaultTempo =
    120.0


tickToMeasure : Int -> Tick -> Measure
tickToMeasure timeBase tick =
    toFloat tick / (toFloat timeBase * 4)


measureToTick : Int -> Measure -> Tick
measureToTick timeBase measure =
    floor (measure * (toFloat timeBase * 4))


tickToTime : Int -> Float -> Tick -> Float
tickToTime timeBase tempo tick =
    timePerTick timeBase tempo * toFloat tick


timePerTick : Int -> Float -> Float
timePerTick timeBase tempo =
    60 / tempo / toFloat timeBase * 1000


timeToTick : Int -> Float -> Float -> Tick
timeToTick timeBase tempo time =
    floor (time / timePerTick timeBase tempo)
