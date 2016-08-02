module Music.Chord exposing (..)

import Music.Note as Note exposing (Note, Interval)


-- TYPES


type Chord
    = Chord Note Quality


type Quality
    = Major
    | Minor



-- FUNCTIONS


transpose : Interval -> Chord -> Chord
transpose interval (Chord note quality) =
    Chord (Note.transpose interval note) quality


toString : Chord -> String
toString (Chord note quality) =
    let
        renderQuality : Quality -> String
        renderQuality quality =
            case quality of
                Major ->
                    ""

                Minor ->
                    "m"
    in
        Note.toString note ++ renderQuality quality
