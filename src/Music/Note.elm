module Music.Note exposing (..)

import List.Extra as List


-- TYPES


type Note
    = Note Int


type alias Interval =
    Int



-- CONSTANTS


notes : List ( Note, String )
notes =
    [ ( noteAb, "Ab" )
    , ( noteA, "A" )
    , ( noteBb, "Bb" )
    , ( noteB, "B" )
    , ( noteC, "C" )
    , ( noteDb, "Db" )
    , ( noteD, "D" )
    , ( noteEb, "Eb" )
    , ( noteE, "E" )
    , ( noteF, "F" )
    , ( noteGb, "Gb" )
    , ( noteG, "G" )
    ]



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
    Note ((int + interval) % 12)


interval : Note -> Note -> Interval
interval (Note oldInt) (Note newInt) =
    (newInt - oldInt) % 12


toString : Note -> String
toString (Note int) =
    let
        noteNames =
            notes |> List.map Tuple.second
    in
        case noteNames |> List.getAt (int % 12) of
            Just noteName ->
                noteName

            Nothing ->
                Debug.crash ("Tried to get an element outside of List (int: " ++ Basics.toString int ++ ").")


fromString : String -> Maybe Note
fromString s =
    notes
        |> List.find (\( _, noteName ) -> noteName == s)
        |> Maybe.map Tuple.first
