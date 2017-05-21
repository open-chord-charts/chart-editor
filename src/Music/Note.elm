module Music.Note exposing (..)

import List.Extra as List
import String


-- TYPES


{-| Af is A flat, As is A sharp
-}
type Note
    = Af
    | A
    | As
    | Bf
    | B
    | Bs
    | Cf
    | C
    | Cs
    | Df
    | D
    | Ds
    | Ef
    | E
    | Es
    | Ff
    | F
    | Fs
    | Gf
    | G
    | Gs


type alias OctaveIndex =
    Int


type alias Interval =
    Int


notes : List Note
notes =
    [ Af, A, As, Bf, B, Bs, Cf, C, Cs, Df, D, Ds, Ef, E, Es, Ff, F, Fs, Gf, G, Gs ]


notesAndStrings : List ( Note, String )
notesAndStrings =
    notes |> List.map (\note -> ( note, toString note ))



-- FUNCTIONS


toOctaveIndex : Note -> OctaveIndex
toOctaveIndex note =
    case note of
        Af ->
            0

        A ->
            1

        As ->
            2

        Bf ->
            2

        B ->
            3

        Bs ->
            4

        Cf ->
            3

        C ->
            4

        Cs ->
            5

        Df ->
            5

        D ->
            6

        Ds ->
            7

        Ef ->
            7

        E ->
            8

        Es ->
            9

        Ff ->
            8

        F ->
            9

        Fs ->
            10

        Gf ->
            10

        G ->
            11

        Gs ->
            0


fromOctaveIndex : OctaveIndex -> Note
fromOctaveIndex octaveIndex =
    let
        selectedNotes =
            [ Af, A, Bf, B, C, Df, D, Ef, E, F, Gf, G ]
    in
        selectedNotes |> getAtUnsafe octaveIndex


interval : Note -> Note -> Interval
interval low high =
    let
        lowOctaveIndex =
            toOctaveIndex low

        highOctaveIndex =
            toOctaveIndex high
    in
        (highOctaveIndex - lowOctaveIndex) % 12


transpose : Interval -> Note -> Note
transpose interval note =
    let
        octaveIndex =
            toOctaveIndex note
    in
        fromOctaveIndex (octaveIndex + interval)


toString : Note -> String
toString note =
    let
        noteStr =
            Basics.toString note
    in
        if noteStr |> String.endsWith "f" then
            (noteStr |> String.dropRight 1) ++ "b"
        else if noteStr |> String.endsWith "s" then
            (noteStr |> String.dropRight 1) ++ "#"
        else
            noteStr


fromString : String -> Maybe Note
fromString s =
    notesAndStrings
        |> List.find (\( _, noteStr ) -> noteStr == s)
        |> Maybe.map Tuple.first



-- UNSAFE LIST FUNCTIONS


getAtUnsafe : Int -> List a -> a
getAtUnsafe index list =
    let
        cyclingIndex =
            index % List.length list
    in
        case list |> List.getAt cyclingIndex of
            Just item ->
                item

            Nothing ->
                Debug.crash ("attempt to reach index " ++ Basics.toString cyclingIndex ++ " of list " ++ (Basics.toString list))
