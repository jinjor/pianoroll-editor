port module Update exposing (..)

import Model exposing (Model)
import Msg exposing (Msg(..))
import Core exposing (..)
import KeyCode


port midiOut : Output -> Cmd msg


type alias Output =
    {}


init : ( Model, Cmd Msg )
init =
    Model.init => Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PianorollEvent key ->
            case ( key.ctrl, key.code ) of
                ( True, 65 ) ->
                    Model.selectAll model => Cmd.none

                _ ->
                    model => Cmd.none

        Start ->
            { model | playing = True } => Cmd.none

        Stop ->
            { model | playing = False } => Cmd.none

        PrevMeasure ->
            { model | currentMeasure = model.currentMeasure - 1 } => Cmd.none

        NextMeasure ->
            { model | currentMeasure = model.currentMeasure + 1 } => Cmd.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
