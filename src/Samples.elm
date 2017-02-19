module Samples exposing (allOfMe)

import Music.Chart exposing (..)
import Music.Chord exposing (..)
import Music.Note exposing (..)


allOfMe : Chart
allOfMe =
    let
        partA =
            Part "A"
                [ Bar [ Chord noteC Major ]
                , BarRepeat
                , Bar [ Chord noteE Major ]
                , BarRepeat
                , Bar [ Chord noteA Major ]
                , BarRepeat
                , Bar [ Chord noteD Minor ]
                , BarRepeat
                ]

        partB =
            Part "B"
                [ Bar [ Chord noteE Major ]
                , BarRepeat
                , Bar [ Chord noteA Minor ]
                , BarRepeat
                , Bar [ Chord noteD Major ]
                , BarRepeat
                , Bar [ Chord noteG Major ]
                , BarRepeat
                ]

        partC =
            Part "C"
                [ Bar [ Chord noteF Major ]
                , Bar [ Chord noteF Minor ]
                , Bar [ Chord noteC Major ]
                , Bar [ Chord noteA Major ]
                , Bar [ Chord noteD Minor ]
                , Bar [ Chord noteG Major ]
                , Bar [ Chord noteC Major ]
                , BarRepeat
                ]
    in
        { title = "All of me"
        , key = Key noteC
        , parts = [ partA, partB, PartRepeat "A", partC ]
        }
