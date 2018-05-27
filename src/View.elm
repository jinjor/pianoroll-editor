module View exposing (..)

import Core exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Midi exposing (Tick)
import Model exposing (Mode(..), Model)
import Msg exposing (Key, Msg(..))
import PianorollView
import Svg as S
import Svg.Attributes as SA


view : Model -> Html Msg
view model =
    div []
        [ viewToolbar model
        , viewPianoroll model
        ]


viewToolbar : Model -> Html Msg
viewToolbar model =
    div [ class "toolbar" ]
        [ viewPrevMeasureButton
        , lazy viewPlayButton (Model.isPlaying model)
        , viewNextMeasureButton
        , lazy arrowButton (model.mode == ArrowMode)
        , lazy penButton (model.mode == PenMode)
        , lazy viewTime (Model.getPlayingPosition model)
        ]


viewPrevMeasureButton : Html Msg
viewPrevMeasureButton =
    button
        [ class "toolbar-button"
        , onClick PrevMeasure
        ]
        [ text "前"
        ]


viewPlayButton : Bool -> Html Msg
viewPlayButton playing =
    button
        [ class "toolbar-button"
        , onClick
            (if playing then
                Stop

             else
                TriggerStart
            )
        ]
        [ text
            (if playing then
                "停止"

             else
                "再生"
            )
        ]


viewNextMeasureButton : Html Msg
viewNextMeasureButton =
    button
        [ class "toolbar-button"
        , onClick NextMeasure
        ]
        [ text "後"
        ]


arrowButton : Bool -> Html Msg
arrowButton selected =
    button
        [ class "toolbar-button"
        , classList [ ( "toolbar-button-selected", selected ) ]
        , onClick SelectArrowMode
        ]
        [ text "矢"
        ]


penButton : Bool -> Html Msg
penButton selected =
    button
        [ class "toolbar-button"
        , classList [ ( "toolbar-button-selected", selected ) ]
        , onClick SelectPenMode
        ]
        [ text "筆"
        ]


viewTime : Tick -> Html Msg
viewTime timeInTick =
    div
        [ class "toolbar-button toolbar-button-timeview"
        ]
        [ text
            (formatTime <|
                Midi.tickToTime Midi.defaultTimeBase Midi.defaultTempo timeInTick
            )
        ]


formatTime : Float -> String
formatTime ms =
    let
        s =
            floor ms // 1000

        ss =
            remainderBy 1000 (floor ms)
    in
    String.fromInt s ++ "." ++ (String.padLeft 3 '0' <| String.fromInt ss)


viewPianoroll : Model -> Html Msg
viewPianoroll model =
    div
        [ class "pianoroll-container"
        , tabindex 1
        , custom
            "keydown"
            (decodeKeyDown
                |> Decode.map PianorollEvent
                |> Decode.map
                    (\msg ->
                        { preventDefault = True
                        , stopPropagation = True
                        , message = msg
                        }
                    )
            )
        ]
        [ PianorollView.view model
        ]


decodeKeyDown : Decoder Key
decodeKeyDown =
    Decode.map3 Key
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "shiftKey" Decode.bool)
        (Decode.field "keyCode" Decode.int)
