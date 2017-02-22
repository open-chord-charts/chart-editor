module Music.Chart exposing (..)

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


getBarsOfPart : PartName -> Chart -> List Bar
getBarsOfPart partName { parts } =
    parts
        |> List.filterMap
            (\part ->
                case part of
                    Part partName1 bars ->
                        if partName == partName1 then
                            Just bars
                        else
                            Nothing

                    PartRepeat _ ->
                        Nothing
            )
        |> List.head
        |> Maybe.withDefault []


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
            BarRepeat


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
    case bar of
        Bar chords ->
            Bar (List.map (Chord.transpose nbSemitones) chords)

        BarRepeat ->
            BarRepeat
