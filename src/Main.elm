module Main exposing (main)

import Html exposing (..)
import Components.ChartCard as ChartCard
import ChartTypes exposing (transposeChart, ChartKey(..), ChromaticNote(..))
import Samples


main : Html msg
main =
    div []
        [ ChartCard.view Samples.allOfMe
        , ChartCard.view <| transposeChart (ChartKey F) Samples.allOfMe
        ]
