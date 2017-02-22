module Music.Chord exposing (..)

import Music.Note as Note exposing (Note, Interval)


-- TYPES


type Chord
    = Chord Note Quality


type Quality
    = Major
    | Minor
    | Seventh


qualities : List Quality
qualities =
    [ Major, Minor, Seventh ]



-- FUNCTIONS


transpose : Interval -> Chord -> Chord
transpose interval (Chord note quality) =
    Chord (Note.transpose interval note) quality


qualityToString : Quality -> String
qualityToString quality =
    case quality of
        Major ->
            ""

        Minor ->
            "m"

        Seventh ->
            "7"


toString : Chord -> String
toString (Chord note quality) =
    Note.toString note ++ qualityToString quality
