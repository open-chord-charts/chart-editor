module Music.Chord exposing (..)

import List.Extra as List
import Music.Note as Note exposing (Note, Interval)


-- TYPES


type Chord
    = Chord Note Quality


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


qualities : List ( Quality, String )
qualities =
    [ ( Major, "" )
    , ( Minor, "m" )
    , ( Augmented, "+" )
    , ( MajorSixth, "6" )
    , ( MinorSixth, "m6" )
    , ( Seventh, "7" )
    , ( MajorSeventh, "Δ" )
    , ( MinorMajorSeventh, "mΔ" )
    , ( MinorSeventh, "m7" )
    , ( SeventhSus4, "7sus4" )
    , ( Diminished, "o" )
    , ( HalfDiminished, "ø" )
    , ( Ninth, "9" )
    , ( MajorMinorNinth, "9b" )
    , ( MinorNinth, "m9" )
    , ( AugmentedNinth, "9+" )
    , ( SixthPlusNinth, "69" )
    , ( AugmentedEleventh, "11+" )
    , ( Thirteenth, "13" )
    , ( Altered, "7alt" )
    ]



-- FUNCTIONS


transpose : Interval -> Chord -> Chord
transpose interval (Chord note quality) =
    Chord (Note.transpose interval note) quality


toString : Chord -> String
toString (Chord note quality) =
    Note.toString note ++ qualityToString quality


qualityToString : Quality -> String
qualityToString quality =
    case qualities |> List.find (\( q, _ ) -> q == quality) of
        Just ( _, s ) ->
            s

        Nothing ->
            Debug.crash ("Tried to get quality: " ++ Basics.toString quality ++ " not in `qualities` list.")


qualityFromString : String -> Maybe Quality
qualityFromString s =
    qualities
        |> List.find (\( _, qualityName ) -> qualityName == s)
        |> Maybe.map Tuple.first
