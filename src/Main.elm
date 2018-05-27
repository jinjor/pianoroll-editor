module Main exposing (..)

import Browser
import Html
import Model exposing (Model)
import Msg exposing (Msg)
import Update
import View


main : Program () Model Msg
main =
    Browser.embed
        { init = \_ -> Update.init
        , update = Update.update
        , subscriptions = Update.subscriptions
        , view = View.view
        }
