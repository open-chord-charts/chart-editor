module Music.Chart exposing (..)

import List.Extra as List
import Music.Chord as Chord exposing (Chord)
import Music.Note as Note exposing (Note, Interval)


-- TYPES


type alias Chart =
    { title : String
    , key : Key
    , parts : List Part
    }


type Part
    = Part PartName (List Bar)
    | PartRepeat PartName


mapPartBars : (List Bar -> List Bar) -> Part -> Part
mapPartBars f part =
    case part of
        Part partName bars ->
            Part partName (f bars)

        PartRepeat _ ->
            part


type alias PartIndex =
    Int


type alias PartName =
    String


isPartRepeat : Part -> Bool
isPartRepeat part =
    case part of
        Part _ _ ->
            False

        PartRepeat _ ->
            True


getBarsOfPartByIndex : PartIndex -> Chart -> List Bar
getBarsOfPartByIndex partIndex chart =
    case List.getAt partIndex chart.parts of
        Nothing ->
            []

        Just part ->
            case part of
                Part _ bars ->
                    bars

                PartRepeat _ ->
                    []


getPartName : Part -> PartName
getPartName part =
    case part of
        Part partName _ ->
            partName

        PartRepeat partName ->
            partName


type Bar
    = Bar (List Chord)
    | BarRepeat


mapBarChords : (List Chord -> List Chord) -> Bar -> Bar
mapBarChords f bar =
    case bar of
        Bar chords ->
            Bar (f chords)

        BarRepeat ->
            bar


type Key
    = Key Note



-- TRANSPOSITION FUNCTIONS


transpose : Key -> Chart -> Chart
transpose newKey ({ key, parts } as chart) =
    let
        interval : Key -> Key -> Interval
        interval (Key oldNote) (Key newNote) =
            Note.interval oldNote newNote
    in
        { chart
            | key = newKey
            , parts = List.map (transposePart (interval key newKey)) parts
        }


transposePart : Int -> Part -> Part
transposePart nbSemitones part =
    case part of
        Part name bars ->
            Part name (List.map (transposeBar nbSemitones) bars)

        _ ->
            part


transposeBar : Int -> Bar -> Bar
transposeBar nbSemitones bar =
    bar
        |> mapBarChords (List.map (Chord.transpose nbSemitones))
