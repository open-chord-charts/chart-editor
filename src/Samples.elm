module Samples exposing (allOfMe)

import Music.Chart exposing (..)
import Music.Chord exposing (..)
import Music.Note exposing (..)


allOfMe : Chart
allOfMe =
    let
        partA =
            Part "A"
                [ Bar ( Chord noteC Major, Nothing, Nothing, Nothing )
                , BarRepeat
                , Bar ( Chord noteE Major, Nothing, Nothing, Nothing )
                , BarRepeat
                , Bar ( Chord noteA Major, Nothing, Nothing, Nothing )
                , BarRepeat
                , Bar ( Chord noteD Minor, Nothing, Nothing, Nothing )
                , BarRepeat
                ]

        partB =
            Part "B"
                [ Bar ( Chord noteE Major, Nothing, Nothing, Nothing )
                , BarRepeat
                , Bar ( Chord noteA Minor, Nothing, Nothing, Nothing )
                , BarRepeat
                , Bar ( Chord noteD Major, Nothing, Nothing, Nothing )
                , BarRepeat
                , Bar ( Chord noteG Major, Nothing, Nothing, Nothing )
                , BarRepeat
                ]

        partC =
            Part "C"
                [ Bar ( Chord noteF Major, Nothing, Nothing, Nothing )
                , Bar ( Chord noteF Minor, Nothing, Nothing, Nothing )
                , Bar ( Chord noteC Major, Nothing, Nothing, Nothing )
                , Bar ( Chord noteA Major, Nothing, Nothing, Nothing )
                , Bar ( Chord noteD Minor, Nothing, Nothing, Nothing )
                , Bar ( Chord noteG Major, Nothing, Nothing, Nothing )
                , Bar ( Chord noteC Major, Nothing, Nothing, Nothing )
                , BarRepeat
                ]
    in
        { title = "All of me"
        , key = Key noteC
        , parts = [ partA, partB, PartRepeat "A", partC ]
        }
