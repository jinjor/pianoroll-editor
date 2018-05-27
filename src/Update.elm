port module Update exposing (..)

import Core exposing (..)
import KeyCode
import Midi
import Model exposing (Mode(..), Model, Note, PlayingState(..))
import Msg exposing (Msg(..))
import Task
import Time exposing (Posix)


port send : List MidiOutEvent -> Cmd msg


type alias MidiMessage =
    List Int


type alias MidiOutEvent =
    { portId : String
    , message : MidiMessage
    , at : Float
    }


init : ( Model, Cmd Msg )
init =
    ( Model.init, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PianorollEvent key ->
            case ( key.ctrl, key.shift, key.code ) of
                ( True, _, 65 ) ->
                    ( Model.selectAllNotes model, Cmd.none )

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
                        (if Model.isPlaying model then
                            Stop

                         else
                            TriggerStart
                        )
                        model

                _ ->
                    ( model, Cmd.none )

        TriggerStart ->
            ( model, Task.perform Start Time.now )

        Start startTime ->
            ( Model.startPlaying startTime model, Cmd.none )

        Stop ->
            ( Model.stopPlaying model, Cmd.none )

        Tick currentTime ->
            case model.playingState of
                NotPlaying _ ->
                    ( model, Cmd.none )

                Playing startTime _ futureNotes ->
                    let
                        ( newFutureNotes, cmd ) =
                            sendNotes
                                startTime
                                currentTime
                                futureNotes
                    in
                    ( { model
                        | playingState = Playing startTime currentTime newFutureNotes
                      }
                    , cmd
                    )

        PrevMeasure ->
            ( { model | currentMeasure = model.currentMeasure - 1 }, Cmd.none )

        NextMeasure ->
            ( { model | currentMeasure = model.currentMeasure + 1 }, Cmd.none )

        MouseDownOnNote id mouse ->
            ( Model.selectNote mouse.ctrl id model, Cmd.none )

        MoveSelectedNotes amount ->
            ( Model.updateSelectedNotes
                (\note -> { note | note = note.note + amount })
                model
            , Cmd.none
            )

        SelectArrowMode ->
            ( { model | mode = ArrowMode }, Cmd.none )

        SelectPenMode ->
            ( { model | mode = PenMode }, Cmd.none )

        SetPosition tick ->
            case model.playingState of
                Playing _ _ _ ->
                    ( model, Cmd.none )

                NotPlaying _ ->
                    ( { model | playingState = NotPlaying tick }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if Model.isPlaying model then
        Time.every 100 Tick

    else
        Sub.none



-- LOGIC & EFFECTS


sendNotes : Posix -> Posix -> List Note -> ( List Note, Cmd Msg )
sendNotes startTime currentTime futureNotes =
    let
        timeBase =
            Midi.defaultTimeBase

        tempo =
            Midi.defaultTempo

        time =
            diffMillis startTime currentTime

        ( newNotes, newFutureNotes ) =
            splitWhile
                (\note ->
                    Midi.tickToTime timeBase tempo note.position < time + 1000.0
                )
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
                        [ MidiOutEvent
                            portId
                            [ 0x90 + channel, note.note, note.velocity ]
                            after
                        , MidiOutEvent
                            portId
                            [ 0x80 + channel, note.note, 0 ]
                            (after + Midi.tickToTime timeBase tempo note.length)
                        ]
                    )
                |> send
    in
    ( newFutureNotes, cmd )
