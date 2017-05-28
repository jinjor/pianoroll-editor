module View exposing (..)

import Model exposing (Model)
import Msg exposing (Msg(..), Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg as S
import Svg.Attributes as SA
import PianorollView
import Json.Decode as Decode exposing (Decoder)


view : Model -> Html Msg
view model =
    div [] [ pianorollView model ]


pianorollView : Model -> Html Msg
pianorollView model =
    div
        [ class "pianoroll-container"
        , tabindex 1
        , onWithOptions
            "keydown"
            { defaultOptions | stopPropagation = True }
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
