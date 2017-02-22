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


getPartNameFromIndex : PartIndex -> Chart -> Maybe PartName
getPartNameFromIndex partIndex { parts } =
    parts
        |> List.getAt partIndex
        |> Maybe.map getPartName


type Bar
    = Bar (List Chord)
    | BarRepeat


type Key
    = Key Note



-- FUNCTIONS


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
