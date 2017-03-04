module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import ChartCard
import Samples


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    List ChartCard.Model


model : Model
model =
    [ ChartCard.init Samples.allOfMe
      -- , ChartCard.init Samples.allOfMe
    ]



-- MSG


type Msg
    = ChartCardMsg Int ChartCard.Msg



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChartCardMsg msgIndex nestedMsg ->
            List.indexedMap
                (\index item ->
                    if index == msgIndex then
                        ChartCard.update nestedMsg item
                    else
                        item
                )
                model



-- VIEW


view : Model -> Html Msg
view model =
    section [ class "flex flex-column min-vh-100" ]
        [ header [ class "tc ph4" ]
            [ h1 [ class "f3 f2-m f1-l fw2 black-90 mv3" ]
                [ text "Open Chords Charts" ]
            , h2 [ class "f5 f4-m f3-l fw2 black-50 mt0 lh-copy" ]
                [ text "Chart viewer and editor" ]
            ]
        , section [ class "ph4-ns flex-auto" ]
            (model
                |> List.indexedMap
                    (\index chartModel ->
                        ChartCard.view chartModel
                            |> Html.map (ChartCardMsg index)
                    )
            )
        , footer [ class "pv4 ph3 ph5-m ph6-l bg-near-black" ]
            [ a
                [ class "f6 ph2 link dim moon-gray"
                , href "https://github.com/open-chords-charts/chart-editor"
                ]
                [ text "Source code on GitHub" ]
            ]
        ]
