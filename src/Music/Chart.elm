module Music.Chart exposing (..)

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


type alias PartName =
    String


type Bar
    = Bar ( Chord, Maybe Chord, Maybe Chord, Maybe Chord )
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
        Bar ( a, b, c, d ) ->
            Bar ( Chord.transpose nbSemitones a, b, c, d )

        BarRepeat ->
            BarRepeat
