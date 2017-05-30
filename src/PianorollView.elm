module PianorollView exposing (..)

import Model exposing (Model, Note)
import Msg exposing (Msg(..), Mouse)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Svg.Events exposing (..)
import Svg.Lazy exposing (..)
import Set exposing (..)
import SvgPath
import Json.Decode as Decode exposing (Decoder)
import Midi exposing (Tick, Measure)
import Time exposing (Time)


view : Model -> Svg Msg
view model =
    svg
        [ width (toString pianorollWidthPx)
        , height (toString pianorollHeightPx)
        ]
        [ g []
            [ viewHorizotalNoteLines
            , viewVerticalMeasureLines
            , viewVerticalCurrentPositionLine model.currentMeasure (Model.getPlayingTime model)
            , viewNotes model.currentMeasure (Model.getNotes model)
            ]
        ]


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
        , y (toString <| noteToY note)
        , width "800"
        , height (toString noteHeightPx)
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


viewVerticalCurrentPositionLine : Int -> Time -> Svg msg
viewVerticalCurrentPositionLine baseMeasure time =
    let
        currentMeasure =
            time
                |> Midi.timeToTick Midi.defaultTimeBase Midi.defaultTempo
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
        , height (toString noteHeightPx)
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
            , x (toString left)
            , y (toString <| noteToY note.note)
            , width "20"
            , height (toString noteHeightPx)
            , on "mousedown" (decodeMouseDown |> Decode.map (MouseDownOnNote note.id))
            ]
            []


decodeMouseDown : Decoder Mouse
decodeMouseDown =
    Decode.map2 Mouse
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "shiftKey" Decode.bool)


measureToPx : Measure -> Px
measureToPx measure =
    measure
        * (toFloat pianorollWidthPx / 4)
        |> floor


isBlack : Int -> Bool
isBlack note =
    Set.member (note % 12) blackNotes


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
