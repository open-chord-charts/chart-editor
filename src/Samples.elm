module Samples exposing (..)

import Music.Chart exposing (..)
import Music.Chord exposing (..)
import Music.Note exposing (..)


grammar : Chart
grammar =
    { title = "Chords chart grammar"
    , key = Key noteC
    , parts =
        [ Part "A"
            [ Bar [ Chord noteC Major ]
            , BarRepeat
            , Bar [ Chord noteA MinorSeventh, Chord noteD Seventh ]
            , BarRepeat
            , Bar [ Chord noteG Minor, Chord noteEb Seventh, Chord noteD Seventh ]
            , Bar [ Chord noteAb Major, Chord noteC Seventh, Chord noteF Minor, Chord noteEb MajorSixth ]
            ]
        ]
    }


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
                , Bar [ Chord noteD HalfDiminished ]
                , Bar [ Chord noteG Major ]
                , Bar [ Chord noteC Major ]
                , BarRepeat
                ]
    in
        { title = "All of me"
        , key = Key noteC
        , parts = [ partA, partB, PartRepeat "A", partC ]
        }
