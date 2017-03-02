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
    div []
        [ header [ class "tc ph4" ]
            [ h1 [ class "f3 f2-m f1-l fw2 black-90 mv3" ]
                [ text "Open Chords Charts" ]
            , h2 [ class "f5 f4-m f3-l fw2 black-50 mt0 lh-copy" ]
                [ text "Chart viewer / editor – "
                , a [ href "https://github.com/open-chords-charts/chart-editor" ]
                    [ text "GitHub" ]
                ]
            ]
        , div [ class "ph4" ]
            (model
                |> List.indexedMap
                    (\index chartModel ->
                        ChartCard.view chartModel
                            |> Html.map (ChartCardMsg index)
                    )
            )
        ]
