module Main exposing (..)

import Html
import Msg exposing (Msg)
import Model exposing (Model)
import Update
import View


main : Program Never Model Msg
main =
    Html.program
        { init = Update.init
        , update = Update.update
        , subscriptions = Update.subscriptions
        , view = View.view
        }
