module Main exposing (main)

import Html exposing (..)
import Components.ChartCard as ChartCard
import Music.Transposition exposing (transposeChart)
import Music.Types exposing (ChartKey(..), ChromaticNote(..))
import Samples


main : Html msg
main =
    div []
        [ ChartCard.view Samples.allOfMe
        , ChartCard.view <| transposeChart (ChartKey F) Samples.allOfMe
        ]
