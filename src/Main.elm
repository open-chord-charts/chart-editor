module Main exposing (main)

import Html exposing (..)


-- import Html.App as App

import Components.ChartCard as ChartCard
import Samples


main : Html ChartCard.Msg
main =
    div []
        [ ChartCard.view Samples.allOfMe
        ]



-- main : Program Never
-- main =
--     App.beginnerProgram
--         { model = Samples.allOfMe
--         , view = view
--         , update = ChartCard.update
--         }
