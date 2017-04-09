module Music.Chord exposing (..)

import Music.Note as Note exposing (Note, Interval)


-- TYPES


type Chord
    = Chord Note Quality


type Quality
    = Major
    | Minor
    | Sixth
    | Seventh
    | MinorSeventh
    | HalfDiminished


qualities : List Quality
qualities =
    [ Major, Minor, Sixth, Seventh, MinorSeventh, HalfDiminished ]



-- FUNCTIONS


transpose : Interval -> Chord -> Chord
transpose interval (Chord note quality) =
    Chord (Note.transpose interval note) quality


toString : Chord -> String
toString (Chord note quality) =
    let
        qualityToString : Quality -> String
        qualityToString quality =
            case quality of
                Major ->
                    ""

                Minor ->
                    "m"

                Sixth ->
                    "6"

                Seventh ->
                    "7"

                MinorSeventh ->
                    "m7"

                HalfDiminished ->
                    "ø"
    in
        Note.toString note ++ qualityToString quality
