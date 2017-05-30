port module Update exposing (..)

import Model exposing (Model, Mode(..), Note)
import Msg exposing (Msg(..))
import Core exposing (..)
import KeyCode
import Time exposing (Time)
import Midi
import Task


port send : List MidiOutEvent -> Cmd msg


type alias MidiMessage =
    List Int


type alias MidiOutEvent =
    { portId : String
    , message : MidiMessage
    , at : Time
    }


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

                ( _, _, 32 ) ->
                    update
                        (if model.playing then
                            Stop
                         else
                            TriggerStart
                        )
                        model

                _ ->
                    model => Cmd.none

        TriggerStart ->
            model
                => Task.perform Start Time.now

        Start startTime ->
            ({ model
                | playing = True
                , startTime = startTime
                , currentTime = startTime
             }
                |> Model.prepareFutureNotes
            )
                => Cmd.none

        Stop ->
            { model | playing = False } => Cmd.none

        Tick currentTime ->
            let
                ( futureNotes, cmd ) =
                    sendNotes
                        model.startTime
                        currentTime
                        model.futureNotes
            in
                { model
                    | currentTime = currentTime
                    , futureNotes = futureNotes
                }
                    => cmd

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
    if model.playing then
        Time.every (100 * Time.millisecond) Tick
    else
        Sub.none



-- LOGIC & EFFECTS


sendNotes : Time -> Time -> List Note -> ( List Note, Cmd Msg )
sendNotes startTime currentTime futureNotes =
    let
        timeBase =
            Midi.defaultTimeBase

        tempo =
            Midi.defaultTempo

        time =
            currentTime - startTime

        ( newNotes, newFutureNotes ) =
            splitWhile
                (\note -> Midi.tickToTime timeBase tempo note.position < time + 1000.0)
                futureNotes

        portId =
            "#01"

        channel =
            0

        cmd =
            newNotes
                |> List.map
                    (\note ->
                        ( Basics.max 0.0 (Midi.tickToTime timeBase tempo note.position - time), note )
                    )
                |> List.concatMap
                    (\( after, note ) ->
                        [ MidiOutEvent portId [ 0x90 + channel, note.note, note.velocity ] after
                        , MidiOutEvent portId [ 0x80 + channel, note.note, 0 ] (after + Midi.tickToTime timeBase tempo note.length)
                        ]
                    )
                |> send
    in
        newFutureNotes => cmd
