module Model exposing (..)

import Dict exposing (Dict)
import Time exposing (Time)
import Midi exposing (Tick, Measure)


type alias Model =
    { notes : Dict Int Note
    , currentMeasure : Int
    , mode : Mode
    , playing : Bool
    , startTime : Time
    , currentTime : Time
    , futureNotes : List Note
    , nextId : Int
    }


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
        |> (\( model, id ) ->
                { model | notes = Dict.insert id (createNote id) model.notes }
           )


init : Model
init =
    Model Dict.empty 0 ArrowMode False 0 0 [] 1
        |> addNote (\id -> Note id 60 127 0 100 False)
        |> addNote (\id -> Note id 62 127 120 100 False)
        |> addNote (\id -> Note id 65 127 240 100 False)
        |> addNote (\id -> Note id 64 127 360 100 False)


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


prepareFutureNotes : Model -> Model
prepareFutureNotes model =
    model.notes
        |> Dict.values
        |> getFutureNotes (toFloat model.currentMeasure)
        |> (\notes -> { model | futureNotes = notes })


getFutureNotes : Measure -> List Note -> List Note
getFutureNotes measure notes =
    let
        from =
            Midi.measureToTick measure
    in
        notes
            |> List.filter (\note -> note.position >= from)
            |> List.sortBy .position


getPlayingTime : Model -> Time
getPlayingTime model =
    model.currentTime - model.startTime
