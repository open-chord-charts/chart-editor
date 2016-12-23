module Main exposing (main)

import Html
import Components.ChartDemoPage


main : Program Never Components.ChartDemoPage.Model Components.ChartDemoPage.Msg
main =
    Html.beginnerProgram
        { model = Components.ChartDemoPage.model
        , view = Components.ChartDemoPage.view
        , update = Components.ChartDemoPage.update
        }
