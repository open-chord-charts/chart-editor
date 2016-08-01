module Music.Transposition exposing (..)

import Music.Types exposing (Chart, ChartKey(..), ChromaticNote(..), Part(..), Bar(..), Chord(..))


transposeChart : ChartKey -> Chart -> Chart
transposeChart newKey ({ key, parts } as chart) =
    let
        computeNbSemitones (ChartKey oldNote) (ChartKey newNote) =
            (notePosition newNote - notePosition oldNote)
                % 12

        nbSemitones =
            computeNbSemitones key newKey
    in
        { chart
            | key = newKey
            , parts = List.map (transposePart nbSemitones) parts
        }


transposePart : Int -> Part -> Part
transposePart nbSemitones part =
    case part of
        Part name bars ->
            Part name (List.map (transposeBar nbSemitones) bars)

        (PartRepeat name) as p ->
            p


transposeBar : Int -> Bar -> Bar
transposeBar nbSemitones bar =
    case bar of
        Bar ( a, b, c, d ) ->
            Bar ( transposeChord nbSemitones a, b, c, d )

        BarRepeat ->
            BarRepeat


transposeChord : Int -> Chord -> Chord
transposeChord nbSemitones (Chord note quality) =
    Chord (transposeChromaticNote nbSemitones note) quality


transposeChromaticNote : Int -> ChromaticNote -> ChromaticNote
transposeChromaticNote nbSemitones note =
    repeat nbSemitones next note


repeat : Int -> (a -> a) -> a -> a
repeat n f v =
    List.foldl (\_ acc -> f acc) v (List.repeat n ())


notePosition : ChromaticNote -> Int
notePosition note =
    case note of
        Ab ->
            0

        A ->
            1

        Bb ->
            2

        B ->
            3

        C ->
            4

        Db ->
            5

        D ->
            6

        Eb ->
            7

        E ->
            8

        F ->
            9

        Gb ->
            10

        G ->
            11


next : ChromaticNote -> ChromaticNote
next note =
    case note of
        Ab ->
            A

        A ->
            Bb

        Bb ->
            B

        B ->
            C

        C ->
            Db

        Db ->
            D

        D ->
            Eb

        Eb ->
            E

        E ->
            F

        F ->
            Gb

        Gb ->
            G

        G ->
            Ab
