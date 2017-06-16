module Music.Chord exposing (..)

import List.Extra as List
import Music.Note as Note exposing (Note, Interval)


-- TYPES


type alias Chord =
    ( Note, Quality )


type Quality
    = Major
    | Minor
    | Augmented
    | MajorSixth
    | MinorSixth
    | Seventh
    | MajorSeventh
    | MinorMajorSeventh
    | MinorSeventh
    | SeventhSus4
    | Diminished
    | HalfDiminished
    | Ninth
    | MajorMinorNinth
    | MinorNinth
    | AugmentedNinth
    | SixthPlusNinth
    | AugmentedEleventh
    | Thirteenth
    | Altered


qualities : List Quality
qualities =
    [ Major
    , Minor
    , Augmented
    , MajorSixth
    , MinorSixth
    , Seventh
    , MajorSeventh
    , MinorMajorSeventh
    , MinorSeventh
    , SeventhSus4
    , Diminished
    , HalfDiminished
    , Ninth
    , MajorMinorNinth
    , MinorNinth
    , AugmentedNinth
    , SixthPlusNinth
    , AugmentedEleventh
    , Thirteenth
    , Altered
    ]


qualitiesAndStrings : List ( Quality, String )
qualitiesAndStrings =
    qualities
        |> List.map (\quality -> ( quality, qualityToString quality ))


qualitiesAndStringsHidingMajor : List ( Quality, String )
qualitiesAndStringsHidingMajor =
    qualities
        |> List.map (\quality -> ( quality, qualityToStringHidingMajor quality ))



-- FUNCTIONS


transpose : Interval -> Chord -> Chord
transpose interval ( note, quality ) =
    ( Note.transpose interval note, quality )


toString : Chord -> String
toString ( note, quality ) =
    Note.toString note ++ (qualityToStringHidingMajor quality)


qualityToString : Quality -> String
qualityToString quality =
    case quality of
        Major ->
            "M"

        Minor ->
            "m"

        Augmented ->
            "+"

        MajorSixth ->
            "6"

        MinorSixth ->
            "m6"

        Seventh ->
            "7"

        MajorSeventh ->
            "Δ"

        MinorMajorSeventh ->
            "mΔ"

        MinorSeventh ->
            "m7"

        SeventhSus4 ->
            "7sus4"

        Diminished ->
            "o"

        HalfDiminished ->
            "ø"

        Ninth ->
            "9"

        MajorMinorNinth ->
            "9b"

        MinorNinth ->
            "m9"

        AugmentedNinth ->
            "9+"

        SixthPlusNinth ->
            "69"

        AugmentedEleventh ->
            "11+"

        Thirteenth ->
            "13"

        Altered ->
            "7alt"


qualityToStringHidingMajor : Quality -> String
qualityToStringHidingMajor quality =
    case quality of
        Major ->
            ""

        _ ->
            qualityToString quality


qualityFromString : String -> Maybe Quality
qualityFromString s =
    qualitiesAndStrings
        |> List.find (\( _, qualityStr ) -> qualityStr == s)
        |> Maybe.map Tuple.first
