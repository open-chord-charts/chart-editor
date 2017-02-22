module Music.Chord exposing (..)

import Music.Note as Note exposing (Note, Interval)


-- TYPES


type Chord
    = Chord Note Quality


type Quality
    = Major
    | Minor


qualities : List Quality
qualities =
    [ Major, Minor ]



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


toString : Chord -> String
toString (Chord note quality) =
    Note.toString note ++ qualityToString quality
