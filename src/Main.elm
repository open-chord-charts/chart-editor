module Main exposing (main)

import Html.App
import Components.ChartDemoPage


main : Program Never
main =
    Html.App.beginnerProgram
        { model = Components.ChartDemoPage.model
        , view = Components.ChartDemoPage.view
        , update = Components.ChartDemoPage.update
        }
