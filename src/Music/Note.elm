module Music.Note
    exposing
        ( Interval
        , Note
        , noteAb
        , noteA
        , noteBb
        , noteB
        , noteC
        , noteDb
        , noteD
        , noteEb
        , noteE
        , noteF
        , noteGb
        , noteG
        , notes
        , transpose
        , interval
        , toString
        , fromString
        )

import Debug


-- TYPES


type Note
    = Note Int


type alias Interval =
    Int



-- CONSTANTS


notes : List Note
notes =
    [ noteAb, noteA, noteBb, noteB, noteC, noteDb, noteD, noteEb, noteE, noteF, noteGb, noteG ]



-- FUNCTIONS


noteAb : Note
noteAb =
    Note 0


noteA : Note
noteA =
    Note 1


noteBb : Note
noteBb =
    Note 2


noteB : Note
noteB =
    Note 3


noteC : Note
noteC =
    Note 4


noteDb : Note
noteDb =
    Note 5


noteD : Note
noteD =
    Note 6


noteEb : Note
noteEb =
    Note 7


noteE : Note
noteE =
    Note 8


noteF : Note
noteF =
    Note 9


noteGb : Note
noteGb =
    Note 10


noteG : Note
noteG =
    Note 11


transpose : Interval -> Note -> Note
transpose interval (Note int) =
    Note <| (int + interval) % 12


interval : Note -> Note -> Interval
interval (Note oldInt) (Note newInt) =
    (newInt - oldInt) % 12


toString : Note -> String
toString (Note int) =
    case int % 12 of
        0 ->
            "Ab"

        1 ->
            "A"

        2 ->
            "Bb"

        3 ->
            "B"

        4 ->
            "C"

        5 ->
            "Db"

        6 ->
            "D"

        7 ->
            "Eb"

        8 ->
            "E"

        9 ->
            "F"

        10 ->
            "Gb"

        11 ->
            "G"

        _ ->
            Debug.crash "Should not reach this point"


fromString : String -> Maybe Note
fromString string =
    case string of
        "Ab" ->
            Just noteAb

        "A" ->
            Just noteA

        "Bb" ->
            Just noteBb

        "B" ->
            Just noteB

        "C" ->
            Just noteC

        "Db" ->
            Just noteDb

        "D" ->
            Just noteD

        "Eb" ->
            Just noteEb

        "E" ->
            Just noteE

        "F" ->
            Just noteF

        "Gb" ->
            Just noteGb

        "G" ->
            Just noteG

        _ ->
            Nothing
