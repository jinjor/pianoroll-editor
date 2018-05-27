module Model exposing (..)

import Core exposing (..)
import Dict exposing (Dict)
import Midi exposing (Measure, Tick)
import Time exposing (Posix)


type alias Model =
    { notes : Dict Int Note
    , currentMeasure : Int
    , mode : Mode
    , playingState : PlayingState
    , nextId : Int
    }


type PlayingState
    = NotPlaying Tick
    | Playing Posix Posix (List Note)


type Mode
    = ArrowMode
    | PenMode


type alias Note =
    { id : Int
    , note : Int
    , velocity : Int
    , position : Tick
    , length : Tick
    , selected : Bool
    }


genId : Model -> ( Model, Int )
genId model =
    ( { model | nextId = model.nextId + 1 }, model.nextId )


addNote : (Int -> Note) -> Model -> Model
addNote createNote model =
    model
        |> genId
        |> (\( model_, id ) ->
                { model_ | notes = Dict.insert id (createNote id) model_.notes }
           )


init : Model
init =
    Model Dict.empty 0 ArrowMode (NotPlaying 0) 1
        |> addNote (\id -> Note id 60 127 (480 + 0) 100 False)
        |> addNote (\id -> Note id 62 127 (480 + 480) 100 False)
        |> addNote (\id -> Note id 65 127 (480 + 960) 100 False)
        |> addNote (\id -> Note id 64 127 (480 + 1440) 100 False)


getNotes : Model -> List Note
getNotes model =
    model.notes
        |> Dict.values


selectNote : Bool -> Int -> Model -> Model
selectNote ctrl id model =
    model
        |> updateNotes
            (Dict.map
                (\_ note ->
                    { note | selected = note.id == id }
                )
            )


selectAllNotes : Model -> Model
selectAllNotes model =
    model
        |> updateNotes
            (Dict.map
                (\id note ->
                    { note | selected = True }
                )
            )


updateSelectedNotes : (Note -> Note) -> Model -> Model
updateSelectedNotes f model =
    model
        |> updateNotes
            (Dict.map
                (\id note ->
                    if note.selected then
                        f note

                    else
                        note
                )
            )


updateNotes : (Dict Int Note -> Dict Int Note) -> Model -> Model
updateNotes f model =
    { model | notes = f model.notes }


startPlaying : Posix -> Model -> Model
startPlaying startTime model =
    let
        futureNotes =
            model.notes
                |> Dict.values
                |> getFutureNotes (toFloat model.currentMeasure)
    in
    { model
        | playingState = Playing startTime startTime futureNotes
    }


isPlaying : Model -> Bool
isPlaying model =
    case model.playingState of
        Playing _ _ _ ->
            True

        _ ->
            False


stopPlaying : Model -> Model
stopPlaying model =
    { model
        | playingState = NotPlaying (getPlayingPosition model)
    }


getFutureNotes : Measure -> List Note -> List Note
getFutureNotes measure notes =
    let
        from =
            Midi.measureToTick Midi.defaultTimeBase measure
    in
    notes
        |> List.filter (\note -> note.position >= from)
        |> List.sortBy .position


getPlayingPosition : Model -> Tick
getPlayingPosition model =
    case model.playingState of
        NotPlaying tick ->
            tick

        Playing startTime currentTime _ ->
            Midi.timeToTick
                Midi.defaultTimeBase
                Midi.defaultTempo
                (diffMillis startTime currentTime)
