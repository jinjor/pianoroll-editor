port module Update exposing (..)

import Model exposing (Model, Mode(..))
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
            case ( key.ctrl, key.shift, key.code ) of
                ( True, _, 65 ) ->
                    Model.selectAllNotes model => Cmd.none

                ( _, True, 38 ) ->
                    update (MoveSelectedNotes 12) model

                ( _, _, 38 ) ->
                    update (MoveSelectedNotes 1) model

                ( _, True, 40 ) ->
                    update (MoveSelectedNotes -12) model

                ( _, _, 40 ) ->
                    update (MoveSelectedNotes -1) model

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

        MouseDownOnNote id mouse ->
            Model.selectNote mouse.ctrl id model => Cmd.none

        MoveSelectedNotes amount ->
            Model.updateSelectedNotes
                (\note -> { note | note = note.note + amount })
                model
                => Cmd.none

        SelectArrowMode ->
            { model | mode = ArrowMode } => Cmd.none

        SelectPenMode ->
            { model | mode = PenMode } => Cmd.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
