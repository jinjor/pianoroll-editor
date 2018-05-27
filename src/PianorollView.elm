module PianorollView exposing (..)

import Core exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Midi exposing (Measure, Tick)
import Model exposing (Model, Note)
import Msg exposing (Mouse, Msg(..))
import Set exposing (..)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Svg.Events exposing (..)
import Svg.Lazy exposing (..)
import SvgPath


view : Model -> Svg Msg
view model =
    svg
        [ width (String.fromInt pianorollWidthPx)
        , height (String.fromInt pianorollHeightPx)
        , on "click" (Decode.map (toSetPositionMsg 0.25 model.currentMeasure) decodeMouse)
        ]
        [ g []
            [ viewHorizotalNoteLines
            , viewVerticalMeasureLines
            , viewVerticalCurrentPositionLine model.currentMeasure (Model.getPlayingPosition model)
            , viewNotes model.currentMeasure (Model.getNotes model)
            ]
        ]


toSetPositionMsg : Measure -> Int -> Mouse -> Msg
toSetPositionMsg quantizeUnit currentMeasure mouse =
    SetPosition
        ((toFloat mouse.offset.x / toFloat pxPerMeasure + toFloat currentMeasure)
            |> quantizeMeasure quantizeUnit
            |> Midi.measureToTick Midi.defaultTimeBase
        )


quantizeMeasure : Measure -> Measure -> Measure
quantizeMeasure quantizeUnit measure =
    toFloat (round (measure / quantizeUnit)) * quantizeUnit


type alias Px =
    Int


viewHorizotalNoteLines : Svg msg
viewHorizotalNoteLines =
    List.range 0 120
        |> List.map viewHorizotalNoteLine
        |> g []


viewHorizotalNoteLine : Int -> Svg msg
viewHorizotalNoteLine note =
    rect
        [ fill
            (if isBlack note then
                "#ddd"

             else
                "#fff"
            )
        , x "0"
        , y (String.fromInt <| noteToY note)
        , width "800"
        , height (String.fromInt noteHeightPx)
        ]
        []


viewVerticalMeasureLines : Svg msg
viewVerticalMeasureLines =
    List.range 0 3
        |> List.map viewVerticalMeasureLine
        |> g []


viewVerticalMeasureLine : Int -> Svg msg
viewVerticalMeasureLine measure =
    viewVerticalLine "#888" (toFloat measure)


viewVerticalCurrentPositionLine : Int -> Tick -> Svg msg
viewVerticalCurrentPositionLine baseMeasure timeInTick =
    let
        currentMeasure =
            timeInTick
                |> Midi.tickToMeasure Midi.defaultTimeBase
    in
    viewVerticalLine "#66f" (currentMeasure - toFloat baseMeasure)


viewVerticalLine : String -> Measure -> Svg msg
viewVerticalLine color measure =
    S.path
        [ stroke color
        , d
            (SvgPath.start (measureToPx measure) 0
                |> SvgPath.v pianorollHeightPx
            )
        , width "800"
        , height (String.fromInt noteHeightPx)
        ]
        []


viewNotes : Int -> List Note -> Svg Msg
viewNotes measureFrom notes =
    notes
        |> List.map (lazy2 viewNote measureFrom)
        |> g []


viewNote : Int -> Note -> Svg Msg
viewNote measureFrom note =
    let
        left =
            note.position
                |> Midi.tickToMeasure Midi.defaultTimeBase
                |> (\measure -> measure - toFloat measureFrom)
                |> measureToPx
    in
    rect
        [ fill
            (if note.selected then
                "#111"

             else
                "#57a"
            )
        , x (String.fromInt left)
        , y (String.fromInt <| noteToY note.note)
        , width "20"
        , height (String.fromInt noteHeightPx)
        , on "mousedown" (decodeMouse |> Decode.map (MouseDownOnNote note.id))
        ]
        []


decodeMouse : Decoder Mouse
decodeMouse =
    Decode.map3 Mouse
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "shiftKey" Decode.bool)
        decodeOffset


decodeOffset : Decoder Position
decodeOffset =
    Decode.map2 Position
        (Decode.field "offsetX" Decode.int)
        (Decode.field "offsetY" Decode.int)


measureToPx : Measure -> Px
measureToPx measure =
    measure
        * toFloat pxPerMeasure
        |> floor


pxPerMeasure : Int
pxPerMeasure =
    pianorollWidthPx // 4


isBlack : Int -> Bool
isBlack note =
    Set.member (remainderBy 12 note) blackNotes


blackNotes : Set Int
blackNotes =
    Set.fromList [ 1, 3, 6, 8, 10 ]


noteToY : Int -> Px
noteToY note =
    pianorollHeightPx - noteHeightPx * note


pianorollWidthPx : Px
pianorollWidthPx =
    600


pianorollHeightPx : Px
pianorollHeightPx =
    480


noteHeightPx : Px
noteHeightPx =
    4
