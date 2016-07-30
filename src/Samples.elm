module Samples exposing (allOfMe)

import ChartTypes exposing (..)


allOfMe : Chart
allOfMe =
    let
        partA =
            Part "A"
                [ Bar ( Chord C Major, Nothing, Nothing, Nothing )
                , BarRepeat
                , Bar ( Chord E Major, Nothing, Nothing, Nothing )
                , BarRepeat
                , Bar ( Chord A Major, Nothing, Nothing, Nothing )
                , BarRepeat
                , Bar ( Chord D Minor, Nothing, Nothing, Nothing )
                , BarRepeat
                ]

        partB =
            Part "B"
                [ Bar ( Chord E Major, Nothing, Nothing, Nothing )
                , BarRepeat
                , Bar ( Chord A Minor, Nothing, Nothing, Nothing )
                , BarRepeat
                , Bar ( Chord D Major, Nothing, Nothing, Nothing )
                , BarRepeat
                , Bar ( Chord G Major, Nothing, Nothing, Nothing )
                , BarRepeat
                ]

        partC =
            Part "C"
                [ Bar ( Chord F Major, Nothing, Nothing, Nothing )
                , Bar ( Chord F Minor, Nothing, Nothing, Nothing )
                , Bar ( Chord C Major, Nothing, Nothing, Nothing )
                , Bar ( Chord A Major, Nothing, Nothing, Nothing )
                , Bar ( Chord D Minor, Nothing, Nothing, Nothing )
                , Bar ( Chord G Major, Nothing, Nothing, Nothing )
                , Bar ( Chord C Major, Nothing, Nothing, Nothing )
                , BarRepeat
                ]
    in
        { title = "All of me"
        , key = ChartKey C
        , parts = [ partA, partB, PartRepeat "A", partC ]
        }
