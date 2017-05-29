module View exposing (..)

import Model exposing (Model, Mode(..))
import Msg exposing (Msg(..), Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Svg as S
import Svg.Attributes as SA
import PianorollView
import Json.Decode as Decode exposing (Decoder)
import Core exposing (..)


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
        , lazy viewPlayButton model.playing
        , viewNextMeasureButton
        , lazy arrowButton (model.mode == ArrowMode)
        , lazy penButton (model.mode == PenMode)
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
        , classList [ "toolbar-button-selected" => selected ]
        , onClick SelectArrowMode
        ]
        [ text "矢"
        ]


penButton : Bool -> Html Msg
penButton selected =
    button
        [ class "toolbar-button"
        , classList [ "toolbar-button-selected" => selected ]
        , onClick SelectPenMode
        ]
        [ text "筆"
        ]


viewPianoroll : Model -> Html Msg
viewPianoroll model =
    div
        [ class "pianoroll-container"
        , tabindex 1
        , onWithOptions
            "keydown"
            { defaultOptions | preventDefault = True, stopPropagation = True }
            (decodeKeyDown |> Decode.map PianorollEvent)
        ]
        [ PianorollView.view model
        ]


decodeKeyDown : Decoder Key
decodeKeyDown =
    Decode.map3 Key
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "shiftKey" Decode.bool)
        (Decode.field "keyCode" Decode.int)
