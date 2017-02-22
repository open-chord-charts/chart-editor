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
        [ h1 []
            [ text "Music chart viewer/editor" ]
        , p []
            [ text "Source code on "
            , a [ href "https://github.com/open-chords-charts/chart-editor" ]
                [ text "GitHub" ]
            ]
        , section []
            (List.indexedMap
                (\index chartModel -> Html.map (ChartCardMsg index) (ChartCard.view chartModel))
                model
            )
        ]
